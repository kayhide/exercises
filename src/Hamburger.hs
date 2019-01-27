{-# LANGUAGE TypeOperators #-}
module Hamburger where

import           ClassyPrelude

data Fail

data Topping = Meat | Bacon | Egg | Lettuce | Tomato | Cheese
  deriving (Eq, Show)


data Burger a = Refl
  deriving (Typeable)

type family (:<<<) (h :: k) (t :: Topping) :: k where
  Burger '[] :<<< t = Burger '[t]
  Burger '[a] :<<< t = Burger '[t, a]
  Burger '[a, b] :<<< t = Burger '[t, a, b]
  Burger '[a, b, c] :<<< t = Burger '[t, a, b, c]
  Burger _ :<<< _ = Fail


type EmptyBurger = Burger '[]
type Hamburger = EmptyBurger :<<< 'Meat
type CheeseBurger = Hamburger :<<< 'Cheese
type DoubleCheeseBurger = CheeseBurger :<<< 'Cheese
type BaconLettuceTomatoBurger = Hamburger :<<< 'Bacon :<<< 'Lettuce :<<< 'Tomato
type BaconEggDoubleCheeseBurger = DoubleCheeseBurger :<<< 'Bacon :<<< 'Egg
