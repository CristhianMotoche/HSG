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
import           SBool
--import           Data.Constraint            (Dict (..))
import           Data.Kind                  (Type)


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
  logMsg _ = pure ()
  runLogging = id

-- In the 'True case we introduce a WriterT [String] over the monad stack
instance MonadLogging 'True where
  type LoggingMonad 'True = WriterT [String] IO
  logMsg msg = tell [msg]
  runLogging mw = do
    (a, msgs) <- runWriterT mw
    mapM_ putStrLn msgs
    return a


program :: MonadLogging b => LoggingMonad b ()
program = do
  logMsg "hello world"
  pure ()


main :: IO ()
main = do
  bool <- read <$> getLine
  withSomeSBool (toSBool bool) $
    \(_ :: SBool b) ->
      runLogging @b program
