module Three where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Eq a
      => EqProp (Tree a) where
  (=-=) = eq

instance Arbitrary a
      => Arbitrary (Tree a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    l <- arbitrary
    r <- arbitrary
    frequency [ (1, return $ Empty)
              , (1, return $ Leaf x)
              , (1, return $ Node l y r)
              ]

instance Functor Tree where
  fmap _ Empty    = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r)
    = Node (fmap f l)
           (f x)
           (fmap f r)

-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr, too,
-- for extra credit.
instance Foldable Tree where
  foldMap _ Empty    = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r)
    = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  traverse _ Empty    = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r)
    = Node <$> traverse f l
           <*> f x
           <*> traverse f r

type SI = Sum Int
type SSI = (String,String,Int,SI)

main :: IO ()
main = do
  let trigger :: Tree SSI
      trigger = undefined
  quickBatch (traversable trigger)
