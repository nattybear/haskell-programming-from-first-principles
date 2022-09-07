module Text.Integers where

import Text.Trifecta

parseInteger :: Parser Integer
parseInteger = do
  x <- integer
  eof
  return x

main :: IO ()
main = do
  print $ parseString parseInteger mempty "123"
  print $ parseString parseInteger mempty "123abc"
