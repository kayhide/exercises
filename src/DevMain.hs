{-# LANGUAGE BlockArguments #-}
module DevMain where

import           ClassyPrelude

import           Test.QuickCheck

import           StateMonad


run :: IO ()
run = do
  let m = State (fst &&& id)

  print $ runState @(Int, Text) (fmap (*10) m) (1283, "Good!")
  print $ runState (fmap (*10) m) (3, True)

  print $ runState (pure (*10) <*> m) (3, True)

  print $ execState
    do
      (i, b) :: (Int, Bool) <- get
      put (i * 2, b)
      b' <- gets $ not . snd
      put (i * 2, b')
    (3, True)

  testProps


-- * Propertiy tests

testProps :: IO ()
testProps = do
  quickCheck prop_leftIdentity
  quickCheck prop_rightIdentity
  quickCheck prop_associativity


newtype ArbitraryState s a = ArbitraryState (State s a)

instance Show a => Show (ArbitraryState s a) where
  show _ = "ArbitraryState (..)"

instance Arbitrary a => Arbitrary (ArbitraryState s a) where
  arbitrary = ArbitraryState . pure <$> arbitrary

prop_leftIdentity :: Int -> Int -> Bool
prop_leftIdentity i s = runState (pure i >>= put) s == runState (put i) s

prop_rightIdentity :: ArbitraryState Int Int -> Int -> Bool
prop_rightIdentity (ArbitraryState m) s = runState (m >>= pure) s == runState m s

prop_associativity :: ArbitraryState Int Int -> Int -> Bool
prop_associativity (ArbitraryState m) s =
  runState ((m >>= f) >>= g) s == runState (m >>= (\x -> f x >>= g)) s
  where
    f  = pure . (* 5)
    g  = pure . (+ 10)
