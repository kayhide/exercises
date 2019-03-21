module Fix where

import ClassyPrelude

import Data.Function


run :: IO ()
run = do
  print $ fib 0
  print $ fib 5
  print $ fib (-2)

fib :: Int -> Int
fib = fix $ \f n -> bool (n * f (n - 1)) 1 (n <= 1)

