module Four where

data Four' a b
  = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d)
    = f b <> f c <> f d
