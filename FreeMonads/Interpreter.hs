module Interpreter where

{-

Toy language:

  output b -- prints a "b" to the console
  bell     -- rings the computer's bell
  done     -- end of execution

-}

data Toy b next =
    Output b next
  | Bell next
  | Done  -- no recursive

{-
- We can represent programs:

output 'A'
done

 => Output 'A' Done :: Toy Char (Toy a next)

- Unfortunately, the type changes:

bell
output 'A'
done

 => Bell (Output 'A' Done) :: Toy a (Toy Char (Toy b next)))

We can create a "cheat" to "hold" the same type

  data Fix f = Fix (f (Fix f))

=> Fix (Output 'A' (Fix Done))              :: Fix (Toy Char)

=> Fix (Bell (Fix (Output 'A' (Fix Done)))) :: Fix (Toy Char)

This approach only works if you can use the `Done` constructor
to terminate every chain of functors.

To work around the above inconvenient, we can throw an exception and let
the caller catch it and resume:
-}

data FixE f e = Fix (f (FixE f e)) | Throw e

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f   = Fix (fmap (flip catch f) x)
catch (Throw e) f = f e


{-
  This works only if `Toy b` is a `Functor`:
-}

instance Functor (Toy b) where
  fmap f (Output x next) = Output x (f next)
  fmap f (Bell     next) = Bell     (f next)
  fmap f  Done           = Done

-- Now, we can write some code that can be caught and resumed:

data IncompleteException = IncompleteException

-- output 'A'
-- throw IncompleteException

subroutine :: FixE (Toy Char) IncompleteException
subroutine = Fix (Output 'A' (Throw IncompleteException))

-- try {subroutine}
-- catch (IncompleteException) {
--     bell
--     done
-- }

program :: FixE (Toy Char) e
program = subroutine `catch` (\_ -> Fix (Bell (Fix Done)))
