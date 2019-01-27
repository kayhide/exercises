{-# LANGUAGE BlockArguments #-}
module StateMonad where

import           ClassyPrelude

import           Test.QuickCheck

newtype State s a = State { runState :: s -> (a, s) }


instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap ab (State sas) = State $ first ab . sas

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure a = State (a,)

  -- <*> :: State s (a -> b) -> State s a -> State s b
  sf <*> sa = State $ run' . runState sf
    where
      run' (f, s) = runState (f <$> sa) s


instance Monad (State s) where
  -- >>= :: State s a -> (a -> State s b) -> State s b
  sa >>= asf = State $ run' . runState sa
    where
      run' (a, s) = runState (asf a) s


get :: State s s
get = State (id &&& id)

put :: s -> State s ()
put s' = State $ const ((), s')

gets :: (s -> a) -> State s a
gets f = f <$> get

modify :: (s -> s) -> State s ()
modify f = put =<< gets f

evalState :: State s a -> s -> a
evalState s = fst . runState s

execState :: State s a -> s -> s
execState s = snd . runState s



-- * Try to run StateMonad

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
