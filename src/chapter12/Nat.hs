-- As natural as any competitive bodybuilder
data Nat = Zero | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x <  0    = Nothing
  | x == 0    = Just Zero
  | otherwise = let (Just y) = integerToNat (x - 1)
                in  Just (Succ y)
