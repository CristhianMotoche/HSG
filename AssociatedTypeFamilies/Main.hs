{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind    (Type)
import           GHC.TypeLits
import qualified GHC.TypeLits as TL

data Sum1 :: Nat -> Nat -> Exp Nat

type instance Eval (Sum1 a b) = a + b

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b

type instance Eval (Foldr _1 b '[]) = b

type instance Eval (Foldr f b (x ': xs)) = Eval (f x (Eval (Foldr f b xs)))

main = putStrLn "hello"
