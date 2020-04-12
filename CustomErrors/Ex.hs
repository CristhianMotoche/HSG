{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

{-
| A type-safe library is of no value if nobody knows how to use it.
-}

-- 1. _GHC.TypeLits_ defines the type _TypeError_ of kind _ERRORMESSAGE → K_
-- Constraint in an instance context
-- Result of a type family
import           GHC.TypeLits

-- Example:
-- Error message for showing a function

instance (
  TypeError
    ( Text "Attempting to show a function (WTF?) of type `"
    :<>: ShowType (a -> b)
    :<>: Text "'"
    :$$: Text "Did you forget to apply an argument?"
    :$$: Text "Are you drunk?"
    )
  ) => Show (a -> b) where
  show = undefined


main = undefined
