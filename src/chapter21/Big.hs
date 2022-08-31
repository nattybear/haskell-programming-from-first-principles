module Big where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Big a b
  = Big a b b
  deriving (Eq, Show)

instance ( Eq a
         , Eq b )
        => EqProp (Big a b) where
  (=-=) = eq

instance ( Arbitrary a
         , Arbitrary b )
        => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary
                  <*> arbitrary
                  <*> arbitrary

instance Functor (Big a) where
  fmap f (Big a b c)
    = Big a (f b) (f c)

instance Monoid a
      => Applicative (Big a) where
  pure x = Big mempty x x

  Big a1 f1 f2 <*> Big a2 x1 x2
    = Big (a1 <> a2) (f1 x1) (f2 x2)

-- When you have more than one
-- value of type b,
-- use Monoid and Applicative
-- for the Foldable and
-- Traversable instances,
-- respectively:
instance Foldable (Big a) where
  foldMap f (Big a b c)
    = f b <> f c

instance Traversable (Big a) where
  traverse f (Big a b c)
    = Big a <$> f b <*> f c

type SI = Sum Int
type SSI = (String,String,Int)
type Foo = (String,String,Int,SI)

main :: IO ()
main = do
  let trigger :: Big String Foo
      trigger = undefined
  quickBatch (traversable trigger)
