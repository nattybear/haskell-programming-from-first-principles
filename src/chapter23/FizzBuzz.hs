module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzList :: [Integer]
             -> DL.DList String
fizzbuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer
          -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

addResult' :: Integer -> State [String] ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzFromTo :: Integer
               -> Integer
               -> [String]
fizzbuzzFromTo start end =
  execState (mapM_ addResult' [end,end-1..start]) []

main :: IO ()
main =
  -- mapM_ putStrLn $ fizzbuzzList [1..100]
  mapM_ putStrLn $ fizzbuzzFromTo 1 100
