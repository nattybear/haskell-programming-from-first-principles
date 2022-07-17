module Parappa where

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) =>
          Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)
