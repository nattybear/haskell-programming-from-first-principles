import Chapter9.Cipher
import Data.Char

vigenere :: IO String
vigenere = do
  message <- getLine
  keyword <- getLine
  let pairs = myZip message keyword
  return $ map shift pairs

shift :: (Char,Char) -> Char
shift (x,y) = caesar y' x
  where y' = ord y - 65

myZip :: String -> String -> [(Char,Char)]
myZip xs ys = myZip' xs (cycle ys)

myZip' :: String -> String -> [(Char,Char)]
myZip' ""     _          = []
myZip' (x:xs) ys'@(y:ys) = case x of
                             ' ' -> (' ', ' ') : myZip' xs ys'
                             _   -> (x, y)     : myZip' xs ys
