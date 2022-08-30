module Pair where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a b
  = Pair a b
  deriving (Eq, Show)

instance ( Eq a
         , Eq b )
        => EqProp (Pair a b) where
  (=-=) = eq

instance ( Arbitrary a
         , Arbitrary b )
        => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Monoid a
      => Applicative (Pair a) where
  pure = Pair mempty 

  Pair a f <*> Pair b x
    = Pair (a <> b) (f x)

instance Foldable (Pair a) where
  foldr f z (Pair x y)
    = f y z

instance Traversable (Pair a) where
  traverse f (Pair x y)
    = Pair x <$> f y

type SI = Sum Int
type SSI = (String,String,Int)
type Foo = (String,String,String,SI)

main :: IO ()
main = do
  let trigger :: Pair String Foo
      trigger = undefined
  quickBatch (traversable trigger)
