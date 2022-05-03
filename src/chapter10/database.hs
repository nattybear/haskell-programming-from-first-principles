import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr go []
  where go (DbDate time) times = time:times
        go _             times = times

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr go []
  where go (DbNumber x) xs = x:xs
        go _            xs = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = foldr max first times
  where times = filterDbDate items
        first = head times

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb items = total / size
  where nums = filterDbNumber items
        total = fromIntegral (sum nums)
        size = fromIntegral (length nums)
