module List where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a
      => Arbitrary (List a) where
  arbitrary = do
    x  <- arbitrary
    xs <- arbitrary
    frequency [ (1, return Nil)
              , (1, return $ Cons x xs)
              ]

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Cons f fs <*> ys =
    (f <$> ys) `append` (fs <*> ys)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons x xs) =
    f x (foldr f z xs)

instance Traversable List where
  traverse _ Nil = pure Nil 
  traverse f (Cons x xs) =
    Cons <$> f x <*> traverse f xs

type SI = Sum Int

main :: IO ()
main = do
  let trigger :: List (SI,SI,SI,SI)
      trigger = undefined
  quickBatch (traversable trigger)
