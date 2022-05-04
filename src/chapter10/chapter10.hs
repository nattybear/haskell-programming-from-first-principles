stops = "pbtdkg"
vowels = "aeiou"

nouns = ["dog", "cat"]
verbs = ["bite", "hug"]

foo :: [(String, String, String)]
foo = [(a,b,c) | a <- nouns, b <- verbs, c <- nouns]
