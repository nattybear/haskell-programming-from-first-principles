module Optional where

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z  Nada   = z
  foldr f z (Yep x) = f x z

  foldl _ z  Nada   = z
  foldl f z (Yep x) = f z x

  foldMap _  Nada   = mempty
  foldMap f (Yep a) = f a

-- |
-- >>> import Data.Monoid
-- >>> foldMap (+1) Nada :: Sum Int
-- Sum {getSum = 0}
-- >>> foldMap (+1) Nada :: Product Int
-- Product {getProduct = 1}
-- >>> foldMap (+1) (Just 1) :: Sum Int
-- Sum {getSum = 2}
