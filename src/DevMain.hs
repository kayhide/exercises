module DevMain where

import ClassyPrelude

import Data.Function
import Data.Typeable

import qualified Fin
import qualified Fix
import qualified Hamburger
import qualified StateMonad


run :: IO ()
run = Fin.run
-- run = Fix.run

