newtype Word' = Word' String
              deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = if n > m then Nothing else Just (Word' s)
  where n = length $ filter (`elem` vowels) s
        m = length s - n
