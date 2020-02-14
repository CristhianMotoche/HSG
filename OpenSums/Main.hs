{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}


import           Control.Monad.Identity
import           Data.Kind              (Type)
import           Data.Maybe
import           Data.Proxy
import           Debug.Trace
import           Fcf
import           GHC.TypeLits           hiding (type (+))
import           System.Random
import           Unsafe.Coerce


data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum
    :: Int
    -> f t
    -> OpenSum f ts


type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts


type Member t ts = KnownNat (Eval (FindElem t ts))


findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))


inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)


prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @t @ts
     then Just $ unsafeCoerce f
     else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

match
  :: forall f ts b. (forall t. f t -> b)
  -> OpenSum f ts
  -> b
match fn (UnsafeOpenSum _ t) = fn t


weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n f) = UnsafeOpenSum (n + 1) f


{- EXAMPLES -}

simpleOs :: OpenSum Identity '[Bool, Int]
simpleOs = inj (Identity True)

os :: OpenSum Identity '[String, Char, Bool, Int]
os = inj (Identity (3 :: Int))

val :: Maybe (Identity Bool)
val = prj os

decOS :: Either (Identity (IO String)) (OpenSum Identity '[String, Char, Bool, Int])
decOS = decompose weakenOS

weakenOS :: OpenSum Identity '[IO String, String, Char, Bool, Int]
weakenOS = weaken os

bazz :: OpenSum Identity '[IO String, String, Char, Bool, Int] -> Identity Int
bazz myOS = fromMaybe (Identity (0 :: Int)) $ prj myOS

manyValues :: Int -> OpenSum Identity '[Int, Char, String]
manyValues 0 = inj (Identity (10 :: Int))
manyValues 1 = inj (Identity 'A')
manyValues 2 = inj (Identity "Haskell is crazy")
manyValues _ = inj (Identity "Nope")

main = do
  val <- randomRIO (0, 4) :: IO Int
  let result = manyValues val
      fromOSmatch = match f result
      f :: Identity val -> Int
      f (Identity _) = 3
      fromOSprj = prj result :: Maybe (Identity String)
  print val
  print fromOSmatch
  print fromOSprj
  print $ bazz ((weaken . weaken . weaken) simpleOs)
