{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module JSONSchema where

import           Control.Monad.Writer
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Kind                  (Type)
import           Data.Text
import           Data.Typeable
import           Data.Vector                (fromList)
import           GHC.Generics
import           GHC.TypeLits
import qualified GHC.TypeLits               as Err

-- Let's generate a JSON Schema for `Person`
data Person = Person
    { name        :: String
    , age         :: Int
    , phone       :: Maybe String
    , permissions :: [Bool]
    }
    deriving (Generic, Show)

  {-
     MORE EXAMPLES:

       CREATE TABLE person (name Text, ...)

  -}

{-

1) gschema doesn't reference `a`
  a cleaner interface is to enable -XAllowAmbiguousTypes and
  later use -XTypeApplications to fill in the desired variable.

2) Writer [Text] -> Track required parameters
   Value -> JSON Schema
-}
class GSchema (a :: Type -> Type) where
  gschema :: Writer [Text] Value

-- HELPERS

-- Merges the properties of two objects
mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object $ a <> b

-- KnownSymbol nm and tells the corresponding term-level string.
emitRequired :: forall nm. KnownSymbol nm => Writer [Text] ()
emitRequired = tell . pure . pack . symbolVal $ Proxy @nm

-- Closed type family to get the string representation
type family ToJSONType (a :: Type) :: Symbol where
  ToJSONType Int = "integer"
  ToJSONType Integer = "integer"
  ToJSONType Float = "number"
  ToJSONType Double = "number"
  ToJSONType String = "string"
  ToJSONType Bool = "boolean"
  ToJSONType [a] = "array"
  ToJSONType a = TypeName a

-- We can use generic metadata to retrieve `a` type’s name.
type family RepName (x :: Type -> Type) :: Symbol where
  RepName (D1 ('MetaData nm _ _ _) _) = nm

type family TypeName (t :: Type) :: Symbol where
    TypeName t = RepName (Rep t)

-- We'll often generate objects like: {"type": "foo"}

makeTypeObj :: forall a. KnownSymbol (ToJSONType a) => Value
makeTypeObj = object
  [ "type" .=
    String (pack . symbolVal $ Proxy @(ToJSONType a))
  ]

-- We'll need a way to create "properties"

makePropertyObj
  :: forall name
  . (KnownSymbol name)
  => Value -> Value
makePropertyObj v = object
  [ pack (symbolVal $ Proxy @name) .= v
  ]


-- Get access to the record name
-- Pattern matching on `M1 S meta (K1 _ a)`.
-- The `S` type is used as a parameter to `M1` to
-- describe record selector metadata.

instance (KnownSymbol nm, KnownSymbol (ToJSONType a))
          => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 a)) where
  gschema = do
    emitRequired @nm -- 1
    pure . makePropertyObj @nm $ makeTypeObj @a -- 2
  {-# INLINE gschema #-}

-- 1. This instance says that the property nm is required.
-- 2.
--
-- Record selector (?):
-- name :: Person -> String
-- name $ Person "name" 12 Nothing []

-- If we have a product of fields, we need to merge them together.

instance (GSchema f, GSchema g) => GSchema (f :*: g) where
    gschema =
      mergeObjects <$> gschema @f <*> gschema @g
    {-# INLINE gschema #-}

-- For sum types, we simply error out a message.
instance (TypeError ('Err.Text "JSON Schema does not support sum types"))
  => GSchema (f :+: g) where
    gschema = error "JSON Schema does not support sum types"
    {-# INLINE gschema #-}

data Foo = Bar | Baz { foo :: Int } deriving (Generic)

{-
   Foo => datatype => D
   Bar, Baz => constructors => C
   foo => record selector => S
-}

-- `M1 C` - metadata for data constructors.
instance GSchema a => GSchema (M1 C _1 a) where
  gschema = gschema @a
  {-# INLINE gschema #-}


-- `M1 D` - type constructors
instance (GSchema a, KnownSymbol nm)
  => GSchema (M1 D ('MetaData nm _1 _2 _3) a) where
  gschema = do
    sch <- gschema @a
    pure $ object
      [ "title" .= (String . pack . symbolVal $ Proxy @nm)
      , "type" .= String "object"
      , "properties" .= sch
      ]
  {-# INLINE gschema #-}

-- run our Writer [Text]
schema
  :: forall a
  . (GSchema (Rep a), Generic a)
  => Value
schema =
  let (v, reqs) = runWriter $ gschema @(Rep a)
   in mergeObjects v $ object
   [ "required" .=
     Array (fromList $ String <$> reqs)
   ]
{-# INLINE schema #-}

-- Let's print the JSON Schema
pp :: (ToJSON a) => a -> IO ()
pp = LC8.putStrLn . encodePretty


-- We need to specify the encoding for strings, list and optinal values
-- each of them correspond to a different base case of `M1 .. K1`

-- Optional values => (no emitRequired)
instance {-# OVERLAPPING #-} (KnownSymbol nm, KnownSymbol (ToJSONType a))
  => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 (Maybe a))) where
  gschema = pure
             . makePropertyObj @nm
             $ makeTypeObj @a
  {-# INLINE gschema #-}

-- Lists
instance {-# OVERLAPPING #-}
  ( KnownSymbol nm
  , KnownSymbol (ToJSONType [a])
  , KnownSymbol (ToJSONType a)
  )
  => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 [a])) where

  gschema = do
    emitRequired @nm
    let inneerType = object [ "items" .= makeTypeObj @a ]
    pure . makePropertyObj @nm
         . mergeObjects inneerType
         $ makeTypeObj @[a]
  {-# INLINE gschema #-}

-- The previous instance force us to change things for Strings, because they're
-- lists of chars

instance {-# OVERLAPPING #-} KnownSymbol nm
  => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 String)) where

  gschema = do
    emitRequired @nm
    pure . makePropertyObj @nm
         $ makeTypeObj @String
  {-# INLINE gschema #-}
