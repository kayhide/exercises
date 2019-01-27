module StateMonad where

import           ClassyPrelude


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
