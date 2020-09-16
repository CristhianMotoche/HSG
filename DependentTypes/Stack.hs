{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}



import           Control.Monad.Trans.Writer
import           Data.Constraint            (Dict (..))
import           Data.Kind                  (Type)
import           SBool


class Monad (LoggingMonad b) => MonadLogging (b :: Bool) where
  type LoggingMonad b = (r :: Type -> Type) | r -> b
  logMsg :: String -> LoggingMonad b ()
  runLogging :: LoggingMonad b a -> IO a

-- r -> b
--  is known as a type family dependency
--  acts as an injectivity annotation
--  if Haskell knows 'LoggingMonad b' it can infer 'b'


-- The 'False case only ignore attempts to log messages
instance MonadLogging 'False where
  type LoggingMonad 'False = IO
  logMsg _ = pure () -- :: String -> IO ()
  runLogging = id -- :: IO a -> IO a

-- In the 'True case we introduce a WriterT [String] over the monad stack
instance MonadLogging 'True where
  type LoggingMonad 'True = WriterT [String] IO
  logMsg msg = tell [msg] -- :: String -> WriterT [String] IO
  runLogging mw = do -- WriterT [String] IO a -> IO a
    (a, msgs) <- runWriterT mw
    mapM_ putStrLn msgs
    return a


program :: MonadLogging b => LoggingMonad b ()
program = do
  logMsg "hello world"
  pure ()

-- The following code does not work,
-- even though it's total and theoretically fine, because:
--  - typeclasses are implemented in GHC as implicitly passed variables
--  - GHC doesn't know the type of `b`

{-
main :: IO ()
main = do
  bool <- read <$> getLine -- True | False ==> 'True | 'False ?
  withSomeSBool (toSBool bool) $
    \(_ :: SBool b) ->
      runLogging @b program
-}

dict :: ( c 'True, c 'False ) => SBool b -> Dict (c b)
dict STrue  = Dict
dict SFalse = Dict

main :: IO ()
main = do
  bool <- read <$> getLine -- True | False ==> 'True | 'False ?
  withSomeSBool (toSBool bool) $ \(sb :: SBool b) ->
    case dict @MonadLogging sb of
      Dict -> runLogging @b program
