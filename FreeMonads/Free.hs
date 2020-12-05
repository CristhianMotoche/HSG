module Free where

import           Control.Exception.Base (throwIO)
import           Control.Monad.Writer   (Writer, execWriter, tell)
import           Interpreter            (Toy (..))


-- FixE already exists as a Free Monad

data Free f r = Free (f (Free f r)) | Pure r

instance (Functor f) => Functor (Free f) where
  -- fmap :: a -> b -> f a -> f b
  fmap f (Pure a) = Pure (f a)
  fmap f (Free x) = Free ((fmap . fmap) f x)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  -- (<*>) :: f (a -> b) -> f a -> f b
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

-- It's a monad as long as `f` is a Functor

instance (Functor f) => Monad (Free f) where
  return = Pure
  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure r) >>= f = f r

-- `Throw` was our `return`
-- `catch` was our `(>>=)`

-- We can use `do` notation with Monad.
-- `Free f` is a Monad, but `Toy` isn't.
-- Therefore, we'll have to convert our commands into `Free (Toy a)`:

{-

output :: a -> Free (Toy a) ()
output x = Free (Output x (Pure ()))

bell :: Free (Toy a) ()
bell = Free (Bell (Pure ()))

done :: Free (Toy a) r
done = Free Done

-}

-- We can abstract the pattern above as follows:

liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)

output x = liftF (Output x ())
bell     = liftF (Bell     ())
done     = liftF  Done

-- Now, we can sequence these primitives using `do`:

subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: Free (Toy Char) r -- r comes from `done`, phantom type?
program = do
  subroutine
  bell
  done

-- The above is PURE DATA
-- Monads are not only associated with effects
-- The above syntax only BUILDS A DATATYPE
-- We can prove it by converting it into a string:

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) = "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x))     = "bell\n" ++ showProgram x
showProgram (Free Done)         = "done\n"
showProgram (Pure r)            = "return " ++ show r ++ "\n"

-- and print it:

printProgram = putStr $ showProgram (program :: Free (Toy Char) ())

-- showProgram is our first interpreter! Our pretty printer interpreter!

ringBell :: IO () -- some obnoxious library would provide this
ringBell = putStrLn "ring!"

interpret :: (Show b) => Free (Toy b) r -> IO ()
interpret (Free (Output b x)) = print b  >> interpret x
interpret (Free (Bell     x)) = ringBell >> interpret x
interpret (Free  Done       ) = return ()
interpret (Pure r)            = throwIO (userError "Improper termination")

main = do
  putStrLn ""
  putStrLn "Pretty printer interpreter:"
  printProgram

  putStrLn ""
  putStrLn "IO interpreter:"
  interpret program

  putStrLn ""
  putStrLn "Testing interpreter:"
  putStrLn $
    if execWriter (interpretTest program) == ["OUTPUT", "BELL", "DONE"]
       then "Pass!!!"
       else "Fail!!!"

-- Can we write a testing interpreter?
-- Writer [String] a
-- assert execWriter (interpretTest program) == ["OUTPUT", "BELL", "DONE"]

interpretTest :: Free (Toy b) r -> Writer [String] ()
interpretTest = undefined
