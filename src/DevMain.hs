module DevMain where

import           ClassyPrelude

import           Data.Proxy
import           Data.Typeable

import qualified StateMonad


run :: IO ()
run = do
  runStateMonad


runStateMonad :: IO ()
runStateMonad = StateMonad.run
