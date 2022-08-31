module Bigger where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bigger a b
  = Bigger a b b b
  deriving (Eq, Show)

instance ( Eq a
         , Eq b )
        => EqProp (Bigger a b) where
  (=-=) = eq

instance ( Arbitrary a
         , Arbitrary b )
        => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Functor (Bigger a) where
  fmap f (Bigger a b c d)
    = Bigger a (f b) (f c) (f d)

instance Monoid a
      => Applicative (Bigger a) where
  pure x = Bigger mempty x x x

  Bigger a1 f1 f2 f3 <*> Bigger a2 x1 x2 x3
    = Bigger (a1 <> a2) (f1 x1) (f2 x2) (f3 x3)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b c d)
    = f b <> f c <> f d

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d)
    = Bigger a <$> f b <*> f c <*> f d

type SI = Sum Int
type SSI = (String,String,Int)
type Foo = (String,String,SI,SI,SI)

main :: IO ()
main = do
  let trigger :: Bigger String Foo
      trigger = undefined
  quickBatch (foldable trigger)
