module Three where

data Three a b c
  = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

data Three' a b
  = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y <> f z
