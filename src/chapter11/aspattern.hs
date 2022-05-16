import Data.Char

isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf []         []     = True
isSubseqOf _          []     = False
isSubseqOf []         _      = True
isSubseqOf xs'@(x:xs) (y:ys) = if x == y
                               then isSubseqOf xs  ys
                               else isSubseqOf xs' ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map (\w -> (w, capitalizeWord w)) s'
  where s' = words s

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs
