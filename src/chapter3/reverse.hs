module Reverse where

rvrs :: String -> String
rvrs x = concat [awesome, is, curry]
  where awesome = drop 9 x
        is = take 4 (drop 5 x)
        curry = take 5 x

main :: IO ()
main = print $ rvrs "Curry is awesome"
