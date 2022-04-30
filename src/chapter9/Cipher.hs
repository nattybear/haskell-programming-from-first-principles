module Cipher where

caesar :: Int -> Char -> Char
caesar n c = alphas !! index'
  where alphas = ['a'..'z']
        pairs = zip alphas [0..]
        index = snd . head . filter (\(a, b) -> a == c) $ pairs
        index' = (index + n) `mod` 26

uncaesar :: Int -> Char -> Char
uncaesar = \n -> caesar (negate n)
