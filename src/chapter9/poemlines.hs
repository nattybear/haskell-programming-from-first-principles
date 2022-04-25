module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "what immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines s = line : myLines (drop 1 rest)
  where line = takeWhile newline s
        rest = dropWhile newline s
        newline = (/= '\n')

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "what immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)
