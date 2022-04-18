import Data.Bool

data DividedResult =
    Result Integer
  | DividedByZero
  deriving Show

dividedBy :: Integer -> Integer -> (DividedResult, DividedResult)
dividedBy _   0     = (DividedByZero, DividedByZero)
dividedBy num denom =
  (Result $ sign * x, y)
  where (Result x, y) = go (abs num) (abs denom) 0
        sign = bool (-1) 1 (num * denom > 0)
        go  n   d count
          | n < d     = (Result count, Result n)
          | otherwise =
              go (n - d) d (count + 1)
