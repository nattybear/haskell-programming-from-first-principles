stops = "pbtdkg"
vowels = "aeiou"

nouns = ["dog", "cat"]
verbs = ["bite", "hug"]

foo :: [(String, String, String)]
foo = [(a,b,c) | a <- nouns, b <- verbs, c <- nouns]

seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))
