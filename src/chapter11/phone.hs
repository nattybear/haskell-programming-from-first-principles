import Data.Char
import Data.Function
import Data.List

data DaPhone = DaPhone [String] deriving Show

phone :: DaPhone
phone = DaPhone
  [ "1"
  , "abc2"
  , "def3"
  , "ghi4"
  , "jkl5"
  , "mno6"
  , "pqrs7"
  , "tuv8"
  , "wxyz9"
  , "^*"
  , "+_0"
  , ".,#"
  ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol OK. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "OK. Do u think I am pretty Lol"
  , "Lol y"
  , "Just making sure rofl ur turn"
  ]

type Digit = Char

type Presses = Int

reverseTaps' :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps' (DaPhone [])     c = []
reverseTaps' (DaPhone (x:xs)) c = if c' `elem` x
                                  then (d, p+1) : reverseTaps' (DaPhone xs) c
                                  else reverseTaps' (DaPhone xs) c
  where c'     = toLower c
        d      = last x
        Just p = elemIndex c' x

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c = if isUpper c
                      then ('*', 1) : reverseTaps' phone c
                      else reverseTaps' phone c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . map (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

mostPopularLetter :: String -> Char
mostPopularLetter s = fst $ foldr1 (\x y -> if snd x > snd y
                                            then x
                                            else y) pairs
  where pairs = map (\c -> (c, fingerTaps (reverseTaps phone c))) s

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord ss = fst $ foldr1 (\x y -> if snd x > snd y
                                       then x
                                       else y) pairs
  where pairs = map (\s -> (s, (fingerTaps $ cellPhonesDead phone s))) ss
