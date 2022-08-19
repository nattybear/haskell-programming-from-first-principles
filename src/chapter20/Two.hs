module Two where

data Two a b
  = Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two x y) = f y
