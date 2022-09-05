module FizzBuzz where

fizzbuzz :: Integer -> String
fizzbuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

main :: IO ()
main =
  mapM_ (putStrLn . fizzbuzz) [1..100]
