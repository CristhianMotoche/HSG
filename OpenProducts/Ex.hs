{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module OpenProducts where
import           Control.Monad.Identity
import           Data.Kind              (Constraint, Type)
import           Data.Proxy             (Proxy (..))
import qualified Data.Vector            as V
import           Fcf                    hiding (Any)
import           Fcf.Data.List
import           GHC.OverloadedLabels   (IsLabel (..))
import           GHC.TypeLits
import           Unsafe.Coerce          (unsafeCoerce)

data Any (f :: k -> Type) where
  Any :: ft -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct
    :: V.Vector (Any f)
    -> OpenProduct f ts

nil :: OpenProduct Identity '[]
nil = OpenProduct V.empty

op :: OpenProduct Identity '[ '("foo", Int) ]
op = insert #foo (Identity (3 :: Int)) nil

data Key (key :: Symbol) = Key

instance (key ~ key') => IsLabel key (Key key') where
  fromLabel = Key

-- insert
  -- :: Key key
  -- -> ft
  -- -> OpenProduct f ts
  -- -> OpenProduct f ('(key, t) ': ts)
-- insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type UniqueKey (key :: k) (ts :: [(k, t)]) = Null =<< Filter (TyEq key <=< Fst) ts

-- INSERT

insert
  :: Eval (UniqueKey key ts) ~ 'True
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type UpdateError key ts =
  (Text "Wait! "
  :<>: ShowType key
  :<>: Text " not present in "
  :<>: ShowType ts)

type family FindElem (key :: Symbol) (ts :: [(Symbol, k)]) where
  FindElem key ts =
    Eval (FromMaybe (TypeError (UpdateError key ts)) =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

-- GET

type LookupType (key :: k) (ts :: [(k, t)]) =
  FromMaybe Stuck =<< Lookup key ts

get
  :: forall key ts f
  . KnownNat (FindElem key ts)
  => Key key
  -> OpenProduct f ts
  -> f (Eval(LookupType key ts))
get _ (OpenProduct v) =
  unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a

-- UPDATE

--type family RequireJustElem (findElem :: Maybe Nat) :: Constraint where
--  RequireJustElem 'Nothing = TypeError UpdateError
--  RequireJustElem ('Just _1) = KnownNat _1
--    Eval (FromMaybe (TypeError UpdateError) =<< FindIndex (TyEq key <=< Fst) ts)

update
  :: forall key ts f t
  . KnownNat (FindElem key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) =
  OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  SetIndex (FindElem key ts) '(key, t) ts

-- DELETE

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Filter (Not <=< TyEq key <=< Fst) ts

delete
  :: forall key ts f
  . KnownNat (FindElem key ts)
  => Key key
  -> OpenProduct f ts
  -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) =
  OpenProduct $ V.ifilter (\idx _ -> findElem @key @ts /= idx) v

-- UPSERT

type UpsertElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) = Case
  [ 'True --> Eval (UpdateElem key t ts)
  , 'False --> '(key, t) ': ts
  ]
  (Eval (Elem key (Eval (Map Fst ts))))

type FindMaybeElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FindIndex (TyEq key <=< Fst) ts)

{-
HINT:
Type family to compute a MAYBE NAT
corresponding to the index of the key in the list of
types, if it exists. Use class instances to lower this
kind to the term-level, and then pattern match on it
to implement upsert.
-}

class FindUpsertElem (a :: Maybe Nat) where
  upsertElem :: Maybe Int

instance FindUpsertElem 'Nothing where
  upsertElem = Nothing

instance (KnownNat int) => FindUpsertElem ('Just int) where
  upsertElem = Just (fromIntegral $ natVal (Proxy @int))

-- findElem :: forall key ts. KnownNat (FindElem key ts) => Int
--findMaybeElem :: forall a key ts . (Eval ((Fcf.<$>) KnownNat (FindMaybeElem key ts))) => Maybe a
findMaybeElem :: forall key ts . FindUpsertElem (FindMaybeElem key ts) => Maybe Int
findMaybeElem = upsertElem @(FindMaybeElem key ts)

upsert
  :: forall key ts f t
  . FindUpsertElem (FindMaybeElem key ts)
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f (Eval (UpsertElem key t ts))
upsert dictKey ft op@(OpenProduct v) =
  case findMaybeElem @key @ts of
    Just idx ->
      OpenProduct $ v V.// [(idx, Any ft)]
--      update dictKey ft op
    Nothing  ->
      OpenProduct $ V.cons (Any ft) v
--      insert dictKey ft op


-- Chapter 12. Custom Errors

type family RequireUniqueKey (result :: Bool) (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) :: Constraint where
  RequireUniqueKey 'True key t ts = ()
  RequireUniqueKey 'False key t ts =
    TypeError (
      'Text "Attempting to add a field named '"
      ':<>: 'Text key
      ':<>: 'Text "' with type "
      ':<>: 'ShowType t
      ':<>: 'Text " to an OpenProduct."
      ':$$: 'Text "But the OpenProduct already has field `"
      ':<>: 'Text key
      ':<>: 'Text "' with type "
      ':<>: 'ShowType (LookupType key ts)
      ':$$: 'Text "Consider using `update' "
      ':<>: 'Text "instead of `insert'."
    )

friendlyInsert
  :: RequireUniqueKey (Eval (UniqueKey key ts)) key t ts
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)
friendlyInsert _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

-- Exercise 12-i Add helpful type errors to OpenProduct’s update and delete functions.

-- Exercise 12-ii Write a closed type family of kind [K] → ERRORMESSAGE
-- that pretty prints a list. Use it to improve the error
-- message from FriendlyFindElem.

-- Exercise 12-iii
-- See what happens when you directly add a TypeError
-- to the context of a function (eg. foo :: TypeError ... => a).
-- What happens? Do you know why?

{-

foo :: (TypeError ('Text "LOL! What are you doing?")) => String
foo = "foo"

-}
