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
import           Data.Kind            (Constraint, Type)
import           Data.Proxy           (Proxy (..))
import qualified Data.Vector          as V
import           Fcf                  hiding (Any)
import           Fcf.Data.List
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.TypeLits
import           Unsafe.Coerce        (unsafeCoerce)

data Any (f :: k -> Type) where
  Any :: ft -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct
    :: V.Vector (Any f)
    -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key
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

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

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
  [ 'True --> UpdateElem key t ts
  , 'False --> '(key, t) ': ts
  ]
  (Eval (Elem key (Eval (Map Fst ts))))

type FindMaybeElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FindIndex (TyEq key <=< Fst) ts)

-- findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findMaybeElem :: forall key ts . (FindMaybeElem key ts)
findMaybeElem = fromIntegral . natVal <$> Proxy @(FindMaybeElem key ts)

upsert
  :: forall key ts f t
  . KnownNat (FindElem key ts)
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f (Eval (UpsertElem key t ts))
upsert _ ft (OpenProduct v) = undefined