{-# LANGUAGE LambdaCase #-}

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe s = unwords $ map (\case Just x  -> x
                                    Nothing -> "a") s'
  where s' = map notThe (words s)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go s'
  where s'          = words s

        go []           = 0
        go [x]          = 0
        go ("the":y:xs) = if vowelInitial y
                          then 1 + go xs
                          else go (y:xs)

        vowelInitial = vowel . head

vowel :: Char -> Bool
vowel = (`elem` "aeiou")

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter vowel
