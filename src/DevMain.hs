module DevMain where

import           ClassyPrelude

import           Data.Typeable

import qualified Hamburger
import qualified StateMonad
import qualified Fin


run :: IO ()
run = Fin.run
