module Four where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four a b c d
  = Four a b c d
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d )
        => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d)
       => EqProp (Four a b c d) where
  (=-=) = eq

instance Functor (Four a b c) where
  fmap f (Four a b c d)
    = Four a b c (f d)

instance ( Monoid a
         , Monoid b
         , Monoid c )
        => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x

  Four a b c f <*> Four m n o x
    = Four (a <> m)
           (b <> n)
           (c <> o)
           (f x)

type SSI = (String,String,Int)

trigger :: Four String String String SSI
trigger = undefined

main :: IO ()
main = quickBatch (applicative trigger)
