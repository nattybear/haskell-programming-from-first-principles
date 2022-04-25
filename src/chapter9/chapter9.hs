myWords :: String -> [String]
myWords "" = []
myWords s = word : myWords (drop 1 rest)
  where word = takeWhile notBlank s
        rest = dropWhile notBlank s
        notBlank = (/= ' ')
