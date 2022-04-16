numbers :: (Num a, Ord a, Num b)
        => a -> b
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1
