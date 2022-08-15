module Nope where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a
  = NopeDotJpg
  deriving (Eq, Show)

-- We're serious. Write it anyway.

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _  = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return  = pure
  _ >>= _ = NopeDotJpg

main :: IO ()
main = do
  let trigger :: Nope (String,String,Int)
      trigger = undefined
  quickBatch (monad trigger)
