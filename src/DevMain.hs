module DevMain where

import           ClassyPrelude

import           Data.Proxy
import           Data.Typeable

import           Hamburger
import qualified StateMonad


run :: IO ()
run = do
  print . typeRep $ Proxy @CheeseBurger
  print . typeRep $ Proxy @DoubleCheeseBurger
  print . typeRep $ Proxy @BaconLettuceTomatoBurger
  print . typeRep $ Proxy @BaconEggDoubleCheeseBurger -- Fail!



runStateMonad :: IO ()
runStateMonad = StateMonad.run
