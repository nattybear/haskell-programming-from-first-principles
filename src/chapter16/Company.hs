module Company where

data Company a b c = DeepBlue a c
                   | Something b

instance Functor (Company a b) where
  fmap f (DeepBlue a c) = DeepBlue a (f c)
  fmap _ (Something b)  = Something b
