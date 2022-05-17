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

capitalizeParagraph :: String -> String
capitalizeParagraph = init . mconcat . insertDot . map capitalizeWord . split

split :: String -> [String]
split "" = []
split s  = word : split (drop 2 rest)
  where word = takeWhile (/= '.') s
        rest = dropWhile (/= '.') s

insertDot:: [String] -> [String]
insertDot []     = []
insertDot (x:xs) = x : ". " : insertDot xs
