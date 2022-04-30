module PoemLines (split) where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "what immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

split :: Char -> String -> [String]
split _ "" = []
split c s = piece : split c (drop 1 rest)
  where piece = takeWhile pred s
        rest = dropWhile pred s
        pred = (/= c)

myLines :: String -> [String]
myLines = split '\n'

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
