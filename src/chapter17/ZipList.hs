module ZipList where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a
  = ZipList' [a]
  deriving (Eq, Show)

instance Arbitrary a
      => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs
            in  take 3000 l
      ys' = let (ZipList' l) = ys
            in  take 3000 l

instance Semigroup a
      => Semigroup (ZipList' a) where
  (<>) = liftA2 (<>)

instance Monoid a
      => Monoid (ZipList' a) where
  mempty = pure mempty

instance Functor ZipList' where
  fmap f (ZipList' xs)
    = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . repeat

  ZipList' fs <*> ZipList' xs
    = ZipList' $ zipWith ($) fs xs

type SSI = (String,String,Int)

trigger :: ZipList' SSI
trigger = undefined

main :: IO ()
main = do
  -- quickBatch (applicative trigger)
  let zl = ZipList' [1 :: Sum Int]
  quickBatch (monoid zl)
