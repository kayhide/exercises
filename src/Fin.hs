module Fin where

import ClassyPrelude

import Data.Typeable

data N where
  Z :: N
  S :: N -> N
  deriving (Eq, Show)

type Zero = 'Z
type One = 'S Zero
type Two = 'S One
type Three = 'S Two
type Four = 'S Three
type Five = 'S Four

data Fin (n :: N) where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

deriving instance Show (Fin n)


run :: IO ()
run = do
  -- print $ (id &&& typeRep) (FZ :: Fin 'Z) -- compile error
  print $ (id &&& typeRep) (FZ :: Fin One)
  print $ (id &&& typeRep) (FZ :: Fin Two)
  -- print $ (id &&& typeRep) (FS FZ :: Fin One) -- compile error
  print $ (id &&& typeRep) (FS FZ :: Fin Two)
  print $ (id &&& typeRep) (FS FZ :: Fin Three)
  print $ (id &&& typeRep) (FS (FS FZ) :: Fin Three)
