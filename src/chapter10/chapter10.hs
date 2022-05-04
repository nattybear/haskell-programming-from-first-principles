stops = "pbtdkg"
vowels = "aeiou"

foo :: [(Char, Char, Char)]
foo = [(a,b,c) | a <- stops, b <- vowels, c <- stops, a == 'p']
