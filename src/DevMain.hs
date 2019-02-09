module DevMain where

import           ClassyPrelude

import           Data.Typeable

import qualified Hamburger
import qualified StateMonad


run :: IO ()
run = Hamburger.run


runStateMonad :: IO ()
runStateMonad = StateMonad.run
