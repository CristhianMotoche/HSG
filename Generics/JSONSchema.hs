{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
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
import           Data.Kind            (Type)
import           Data.Text
import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits


{-

1) gschema doesn't reference `a`
  a cleaner interface is to enable -XAllowAmbiguousTypes and
  later use -XTypeApplications to fill in the desired variable.

2) Writer [Text] -> Track required parameters
   Value -> JSON Schema
-}
class GSchema (a :: Type -> Type) where
  gschema :: Writer [Text] Value

-- Helpers

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
