{-# LANGUAGE InstanceSigs #-}

module State where

newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) =
    State $ \s ->
      let (x, s1) = g s
      in  (f x, s1)

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State $ \s -> (x, s)

  (<*>) :: State s (a -> b)
        -> State s a
        -> State s b
  State f <*> State g = State $ \s ->
    let (h, s1) = f s
        (x, s2) = g s1
    in  (h x, s2)

instance Monad (State s) where
  return = pure

  (>>=) :: State s a
        -> (a -> State s b)
        -> State s b
  State f >>= g = State $ \s ->
    let (x, s1) = f s
        State h = g x
    in  h s1

get :: State s s
get = State $ \s -> (s, s)

-- |
-- >>> runState get "curryIsAmaze"
-- ("curryIsAmaze","curryIsAmaze")

put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- |
-- >>> runState (put "blah") "woot"
-- ((),"blah")

exec :: State s a -> s -> s
exec (State sa) s = 
  let (x, s') = sa s
  in  s'

-- |
-- >>> exec (put "wilma") "daphne"
-- "wilma"
-- >>> exec get "scooby papu"
-- "scooby papu"

eval :: State s a -> s -> a
eval (State sa) s =
  let (x, s') = sa s
  in   x

-- |
-- >>> eval get "bunnicula"
-- "bunnicula"
-- >>> eval get "stake a bunny"
-- "stake a bunny"

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- |
-- >>> f = modify (+1)
-- >>> runState f 0
-- ((),1)
-- >>> runState (f >> f) 0
-- ((),2)
