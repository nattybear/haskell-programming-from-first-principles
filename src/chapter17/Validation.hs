module Validation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation' e a
  = Failure' e
  | Success' a
  deriving (Eq, Show)

instance ( Arbitrary e
         , Arbitrary a
         , Monoid e )
        => Arbitrary (Validation' e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency [ (1, return $ Failure' e)
              , (1, return $ Success' a) ]

instance (Eq e, Eq a)
      => EqProp (Validation' e a) where
  (=-=) = eq

-- same as Either
instance Functor (Validation' e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' (f a)

-- This is different
instance Monoid e
      => Applicative (Validation' e) where
  pure = Success'

  Failure' e1 <*> Failure' e2 = Failure' (e1 <> e2)
  Success' f  <*> Success' x  = Success' (f x)
  Failure' e  <*> _           = Failure' e
  _           <*> Failure' e  = Failure' e

type SSI = (String,String,Int)

type Foo = Validation' String SSI

trigger :: Foo
trigger = undefined

main :: IO ()
main = quickBatch (applicative trigger)
