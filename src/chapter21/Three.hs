module Three where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance ( Eq a
         , Eq b
         , Eq c )
        => EqProp (Three a b c) where
  (=-=) = eq

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c )
        => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance Functor (Three a b) where
  fmap f (Three x y z)
    = Three x y (f z)

instance ( Monoid a
         , Monoid b )
        => Applicative (Three a b) where
  pure x = Three mempty mempty x

  Three a1 b1 f <*> Three a2 b2 x
    = Three (a1 <> a2) (b1 <> b2) (f x)

instance Foldable (Three a b) where
  foldr f z (Three _ _ x) = f x z

instance Traversable (Three a b) where
  traverse f (Three x y z)
    = Three x y <$> f z

type SSI = (String,String,Int)
type Foo = (String,String,String,Sum Int)

main :: IO ()
main = do
  let trigger :: Three String String Foo
      trigger = undefined
  quickBatch (traversable trigger)
