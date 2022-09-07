module LearnParsers where

import Control.Applicative
import Text.Parser.Combinators
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1'

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop

-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2'

-- read two characters,
-- '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo'"
  testParse oneTwo'

-- |
-- >>> p123 "1"
-- Success 1
-- >>> p123 "12"
-- Success 12
-- >>> p123 "123"
-- Success 123

p123 :: String -> IO ()
p123 s =
  print $ parseString p123' mempty s

p123' :: Parser String
p123' =
      string "123"
  <|> string "12"
  <|> string "1"

string' :: String -> Parser String
string' "" = return ""
string' (x:xs) = do
  c  <- char x
  cs <- string' xs
  return (c:cs)
