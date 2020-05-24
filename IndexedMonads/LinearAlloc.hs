{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


import           Control.Monad.Indexed
import           Data.Coerce
import           Fcf
import           GHC.TypeLits                (Nat)
import qualified GHC.TypeLits                as TL
import           Ix
import           Language.Haskell.DoNotation
import           Prelude                     hiding (Monad (..), pure, (+))
import           System.IO                   hiding (Handle, openFile)
import qualified System.IO                   as SIO

{-
   What we want to do:
   1) Open file handles
   2) And close them exactly ONCE

   How?
   1) Tack whether a file handle is open or closed
   2) We'll keep the open ones in a list and we'll remove then once closed
   3) We'll use a increasing NAT to keep unique kyes for the file handles
-}

-- LinearState exists solely to be used as a data kind.
-- It will be the "index" of our indexed Monad
data LinearState = LinearState
    { linearNextKey  :: Nat
    , linearOpenKeys :: [Nat]
    }

newtype Linear s (i :: LinearState) (j :: LinearState) a = -- (1)
  Linear {
    unsafeRunLinear :: Ix IO i j a -- (2)
   } deriving (IxFunctor, IxPointed, IxApplicative, IxMonad)

{-
  (1) `s` is used with the ST trick
  (2) unsafeRunLinear is unsafe in two ways:
  - lets us run arbitarry Linear computations, including incomplete ones
  - doesn't existentialize `s`, meaning a file handle can leak out of it
-}

openFile
  :: FilePath
  -> IOMode
  -> Linear s
       ('LinearState next open) -- (1)
       ('LinearState (next TL.+ 1) (next ': open)) -- (2) ^ (3)
       (Handle s next)
openFile = coerce SIO.openFile

{-
  (1) It can be used for any `next` and `open`
  (2) the postconditions of `openFile` is that `next` is incremented
  (3) and insert `next` into the open set
-}

newtype Handle s key = Handle
  { unsafeGetHandle :: SIO.Handle
  }

-- to determine whether or not a Handle is already in the open set, we define this FCF:
type IsOpen (key :: k) (ts :: [k])
  = IsJust =<< Find (TyEq key) ts

-- also, we'll need a FCF to compute the result of removing a handle from the open set:
type Close (key :: k) (ts :: [k])
  = Filter (Not <=< TyEq key) ts


closeFile
  :: Eval (IsOpen key open) ~ 'True
  => Handle s key
  -> Linear s
       ('LinearState next open)
       ('LinearState next (Eval (Close key open))) -- (1)
       ()
closeFile = coerce SIO.hClose

{-
   (1) We don't need to decrement the handle count since `next` is used only
       to generate a unque NAT for any new opened handle
-}


runLinear
  :: (forall s.
      Linear s ('LinearState 0 '[]) -- (1)
               ('LinearState n '[]) -- (2)
               a
     )
     -> IO a
runLinear = coerce

{-
   (1) Assuming it had no open files to begin with
   (2) It’s safe to run a Linear if its final state has no open files
-}

etcPasswd = openFile "/etc/passwd" ReadMode

--result = runLinear (etcPasswd >>= closeFile)
-- These operations throw a type error:
--
-- Can't simply open a handle
--
--result = runLinear etcPasswd
--
--
-- Can't close a handle twice
--
--
--result = runLinear $ do
--  handle <- etcPasswd
--  closeFile handle
--  closeFile handle
--
-- ST trick, do your thing:
--
--result = runLinear $ do
--  handle <- etcPasswd
--  closeFile etcPasswd
--  pure handle
