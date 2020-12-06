{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}

module Concurrency where

import           Control.Monad (liftM)


-- How to interleave two monadic 'threads'?
--  For `IO` we could use `forkIO`
--  For `State`, or `Cont`?

-- We could represent a thread as a LIST of INDIVIDUAL MONAD ACTIONS:
--
--  type Thread m = [m ()]
--
-- but it DOESN'T GUARANTEE that our interpreter will call them in the order
-- we list them.

-- We can enforce ordering
--  - by nesting each subsequent action within the previous one,
--  - and we use a separate constructor to indicate we are done (no actions left)

data Thread m r = Atomic (m (Thread m r)) | Return r deriving (Functor, Applicative)


-- We can convert ANY SINGLE MONAD INVOCATION into an atomic `Thread` step:
atomic :: (Monad m) => m a -> Thread m a
atomic m = Atomic $ liftM Return m


-- Now we need a way to make Thread a monad,
-- but we will just "pretend" that we sequence two threads
-- while still keeping their atomic steps separate
-- so that we can later interleave them with other threads.

instance (Monad m) => Monad (Thread m) where
  return = Return
  (Atomic m) >>= f = Atomic (liftM (>>= f) m)
  (Return r) >>= f = f r

thread1 :: Thread IO ()
thread1 = do
  atomic $ print 1
  atomic $ print 2

thread2 :: Thread IO ()
thread2 = do
  str <- atomic $ getLine
  atomic $ putStrLn str


-- All we are missing is a way to interleave two threads,
-- while still maintaining the atomicity of the individual steps.
-- Let's just do a naive alternation:

interleave :: (Monad m) => Thread m r -> Thread m r -> Thread m r
interleave (Atomic m1) (Atomic m2) = do
    next1 <- atomic m1
    next2 <- atomic m2
    interleave next1 next2
interleave t1 (Return _) = t1
interleave (Return _) t2 = t2


-- Now we need a way to run threads after we are done interleaving them:
runThread :: (Monad m) => Thread m r -> m r
runThread (Atomic m) = m >>= runThread
runThread (Return r) = return r

main = runThread (interleave thread1 thread2)
