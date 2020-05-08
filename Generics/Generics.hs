{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

import           GHC.Generics


-- 1. Define a typeclass to act as a carrier.
class GEq a where
  geq :: a x -> a x -> Bool

-- 2. Provide inductive instances of the class for the
-- generic constructors.
--
instance GEq U1 where
  geq U1 U1 = True

instance GEq V1 where
  geq _ _ = True
instance Eq a => GEq (K1 _1 a) where
  geq (K1 a) (K1 b) = a == b

instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a1) (L1 a2) = geq a1 a2
  geq (R1 b1) (R1 b2) = geq b1 b2
  geq _ _             = False

instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

instance GEq a => GEq (M1 _x _y a) where
  geq (M1 a1) (M1 a2) = geq a1 a2

-- 3. Finally, write a helper function to map between the

-- Rep and the desired type.

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

-------------
-------------

data Foo a b c = F0
    | F1 a
    | F2 b c
    deriving (Generic)

instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
  (==) = genericEq

-------------------------------
-------------------------------

class GOrd a where -- Eq instance????
  gcompare :: a x -> a x -> Ordering


instance GOrd U1 where
  gcompare U1 U1 = EQ

instance GOrd V1 where
  gcompare _ _ = EQ

instance Ord a => GOrd (K1 _1 a) where
  gcompare (K1 a) (K1 b) = a `compare` b

instance (GOrd a, GOrd b) => GOrd (a :+: b) where
  gcompare (L1 a1) (L1 a2) = gcompare a1 a2
  gcompare (R1 b1) (R1 b2) = gcompare b1 b2
  gcompare (L1 _) (R1 _)   = LT -- comparing different constructors on sum types
  gcompare (R1 _) (L1 _)   = GT -- comparing different constructors on sum types

instance (GOrd a, GOrd b) => GOrd (a :*: b) where
  gcompare (a1 :*: b1) (a2 :*: b2) = gcompare a1 a2 `compare` gcompare b1 b2

instance GOrd a => GOrd (M1 _x _y a) where
  gcompare (M1 a1) (M1 a2) = gcompare a1 a2

genericCmp :: (Generic a, GOrd (Rep a)) => a -> a -> Ordering
genericCmp a b = gcompare (from a) (from b)
