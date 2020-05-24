{-# LANGUAGE RebindableSyntax #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


import           Data.Coerce
import           Fcf
import           GHC.TypeLits                (Nat)
import qualified GHC.TypeLits                as TL
import           Ix
import           Language.Haskell.DoNotation
import           Prelude                     hiding (Monad (..), pure)
import           System.IO                   hiding (Handle, openFile)
import qualified System.IO                   as SIO
