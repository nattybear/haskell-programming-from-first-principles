data CountingBad a = Heisenberg Int a
                   deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)
