{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Machinery where

-- Let's start with a poly-kinded (many kinds) open data family:

data family Sing (a :: k)

-- Eliminator

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing
  :: SomeSing k
  -> (forall (a :: k). Sing a -> r)
  -> r
withSomeSing (SomeSing s) f = f s


class SingKind k where
  type Demote k = r | r -> k -- (1)
  toSing :: Demote k -> SomeSing k
  fromSing :: Sing (a :: k) -> Demote k

-- (1) TypeInType => removes the DISTINCTION between types and kinds

data instance Sing (a :: Bool) where  -- GADTs replacement
  STrue :: Sing 'True
  SFalse :: Sing 'False

instance SingKind Bool where
  type Demote Bool = Bool
  toSing True  = SomeSing STrue
  toSing False = SomeSing SFalse
  fromSing STrue  = True
  fromSing SFalse = False


-- Now, try:
--
-- withSomeSing (toSing True) fromSing
-- withSomeSing (toSing False) fromSing


-- singletons are the unique inhabitant of their types
-- at the term-level they are isomorphic with ()
-- We can get this unique inhabitant, as we can always conjure a ()

class SingInhabitant (a :: k) where
  sing :: Sing a

instance SingInhabitant 'True where
  sing = STrue

instance SingInhabitant 'False where
  sing = SFalse

-- Now, try:
--
-- :t sing @'True

data instance Sing (a :: Maybe k) where
  SJust    :: Sing (a :: k) -> Sing ('Just a)
  SNothing :: Sing 'Nothing

instance (k ~ Demote k, SingKind k) => SingKind (Maybe k) where
  type Demote (Maybe k) = Maybe k
  toSing (Just a) = withSomeSing (toSing a) $ SomeSing . SJust
  toSing Nothing  = SomeSing SNothing
  fromSing (SJust a) = Just $ fromSing a
  fromSing SNothing  = Nothing

instance SingInhabitant a => SingInhabitant ('Just a) where
  sing = SJust sing

instance SingInhabitant 'Nothing where
  sing = SNothing

data instance Sing (a :: [k]) where
  SNil  :: Sing '[]
  SCons :: Sing (h :: k)
        -> Sing (t :: [k])
        -> Sing (h ': t)


instance (k ~ Demote k, SingKind k) => SingKind [k] where
  type Demote [k] = [k]
  toSing [] = SomeSing SNil
  toSing (h : t) =
    withSomeSing (toSing h) $ \sh ->
      withSomeSing (toSing t) $ \st ->
        SomeSing $ SCons sh st
  fromSing SNil = []
  fromSing (SCons sh st) =
    fromSing sh : fromSing st

instance SingInhabitant '[] where
  sing = SNil

instance (SingInhabitant a, SingInhabitant as) => SingInhabitant (a ': as) where
  sing = SCons sing sing
