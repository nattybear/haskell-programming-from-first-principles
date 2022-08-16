module BahEither where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data BahEither b a
  = PLeft a
  | PRight b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
       => Arbitrary (BahEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return $ PLeft x)
              , (1, return $ PRight y) ]

instance (Eq a, Eq b)
       => EqProp (BahEither b a) where
  (=-=) = eq

instance Functor (BahEither b) where
  fmap f (PLeft x)  = PLeft (f x)
  fmap _ (PRight x) = PRight x

instance Applicative (BahEither b) where
  pure = PLeft

  PLeft f  <*> PLeft x  = PLeft (f x)
  PRight x <*> _        = PRight x
  _        <*> PRight x = PRight x

instance Monad (BahEither b) where
  return = pure

  PLeft x  >>= f = f x
  PRight x >>= _ = PRight x

main :: IO ()
main = do
  let trigger :: BahEither String (String,String,Int)
      trigger = undefined
  quickBatch (monad trigger)
