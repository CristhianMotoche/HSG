{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}


module Ix where

import           Control.Monad.Indexed
import           Data.Coerce

{-

Indexed Mondas are generalization of monads that
allow us to enforce `pre-` and `post-` conditions on monadic actions.

Useful for:
- Protocols & Contracts at the type level

In the rest of the chapter we'll look at:
- how to statically enforce resource allocation linearity.


-}

{-
class IxApplicative m => IxMonad m where
  ibind :: (a -> m j k b) -> m i j a -> m i k b

i j k => “state” of the monad at different times

An indexed monadic action m i j a is one that
- produces an `a`,
- with precondition `i`
- and post-condition `j`

-}

{-
  `ibind`
      matches the post-condition of `m i j` (`i`)
      with the precondition of `m j k` (`k`)
      the intermediary condition `j` is eliminated,
-}


{-
  This helper will help us to 'lift' an underlying monad to have
  this indexed structure.
-}

newtype Ix m i j a = Ix
  { unsafeRunIx :: m a
  } deriving (Functor, Applicative, Monad)

{-
  Parameters of Ix:
  - m -> underyling monad we want to lift into an indexed monad
  - i -> preconditions
  - j -> post-conditions
  - a -> type we produce at the end
-}

{-
   Indexed monads have their own indexed-version of the standard
   typeclass hierarchy. Let's define that for `Ix`:
-}

instance Functor m => IxFunctor (Ix m) where
  imap = fmap -- In terms of Functor because the types don't conflict

instance Applicative m => IxPointed (Ix m) where
  ireturn = pure -- In terms of Functor because the types don't conflict

{-
   We can `coerce` the usual Applicative function into the right shape since
   `i`, `j` and `k` are phanthom variables.
-}

instance Applicative m => IxApplicative (Ix m) where
  iap :: forall i j k a b. -- (1)
         Ix m i j (a -> b)
      -> Ix m j k a
      -> Ix m i k b
  iap = coerce $ (<*>) @m @a @b -- (2)

{-
   (1) We need to euse InstanceSigs in order to capture `a` and `b` with
   `ScopedTypeVariables`.
   (2) Now, `<*>` is trivially coerced.


   Same for monads:
-}

instance Monad m => IxMonad (Ix m) where
  ibind
    :: forall i j k a b
    . (a -> Ix m j k b)
    -> Ix m i j a
    -> Ix m i k b
  ibind = coerce $ (=<<) @m @a @b
