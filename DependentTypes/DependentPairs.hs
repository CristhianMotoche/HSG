{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE UndecidableInstances  #-}

module DependentPairs where

import           Data.Aeson
import           Data.Constraint
import           Data.Kind               (Type)
import           Data.Maybe              (mapMaybe)
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           Machinery               hiding (Demote, Sing, SingKind,
                                          fromSing, sing)


-- Sigma types => Dependent Pairs
--  - They generalize arbitrarily-deeply nested Either types
--    - Either (Either (Either (Either (...) b) c) d) e ?
--  - Propositions as types
--    - existential quantifier ∃

-- Sigma types are the pair of an
--  - existential singleton
--  - type indexed by that singleton
--
--  - type families + singletons ? XXX

data Sigma (f :: k -> Type) where
  Sigma :: Sing a -> f a -> Sigma f


{-
  Sigma takes
    - a singleton for an existential type `a` => `Sing a`
    - and datatype `f a` (of kind K) => `f a`
      - `f a` is parameterized on the existential type `a`
-}

withSigma
  :: (forall (a :: k). Sing a -> f a -> r)
  -> Sigma f
  -> r
withSigma c (Sigma s f) = c s f


-- `toSigma` lifts an arbitrary `f a` into a `Sigma f`

toSigma
  :: SingI a
  => f a
  -> Sigma f
toSigma = Sigma sing
-- toSigma fa = Sigma sing fa


-- `s` inside `Sigma` can help us to `cast` `Sigma f` into `f a`

fromSigma
  :: forall k (a :: k) (f :: k -> Type). (SingI a, SDecide k)
  => Sigma f
  -> Maybe (f a)
fromSigma (Sigma s f) =
  case s %∼ sing @a of
    Proved Refl -> Just f
    Disproved _ -> Nothing


-- The `dict` function from our logging example can also
-- be generalized into a typeclass capable of providing total
-- constraints given a singleton.
class Dict1 c (f :: k -> Type) where
  dict1 :: Sing (a :: k) -> Dict (c (f a))

-- c :: Type -> Constraint
-- f :: K -> Type
-- a :: K

instance (Dict1 Eq (f :: k -> Type), SDecide k) => Eq (Sigma f) where
  Sigma sa fa == Sigma sb fb =
    case sa %∼ sb of
      Proved Refl ->
        case dict1 @Eq @f sa of
          Dict -> fa == fb
      Disproved _ -> False

instance (Dict1 Show (f :: k -> Type), Show (Demote k), SingKind k)
  => Show (Sigma f) where
    show (Sigma sa fa) =
      case dict @Show @f sa of
        Dict -> mcontact
          [ "Sigma "
          , show $ fromSing sa
          , " ("
          , show fa
          , ")"
          ]
