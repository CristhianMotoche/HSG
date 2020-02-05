{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}


module ExistetialsEliminators where

import           Data.Foldable

{-
  A type that contains another type only inside its scope.
  The inside type only lives within the context of the data constructor.
-}

data Any where
  Any :: a -> Any

anyList :: [Any]
anyList = [Any 3, Any 5, Any "asdf"]

anyInt :: Any
anyInt = Any 4

{-
   A eliminator (consumer) is a function of rank 2.
   which takes an existential type and a continuation
   that can produce a value regardless of what it gets.
-}

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

data HasShow where
  HasShow :: Show t => t -> HasShow

instance Show HasShow where
  show hs@(HasShow s) = "HasShow " ++ elimHasShow show hs

hs :: HasShow
hs = HasShow "LOL"

elimHasShow
  :: (forall a. Show a => a -> r)
  -> HasShow
  -> r
elimHasShow f (HasShow a) = f a

main :: IO ()
main = print $ asum [Nothing, Just "asdf", Just "fads"]
