module Chapter9.Cipher where

-- uncaesar :: Int -> Char -> Char
-- uncaesar = \n -> caesar (negate n)

caesar :: IO Char
caesar = do
  n   <- read <$> getLine :: IO Int
  [c] <- getLine
  let alphas = ['A'..'Z']
  let pairs  = zip alphas [0..]
  let index  = snd . head . filter (\(a, b) -> a == c) $ pairs
  let index' = (index + n) `mod` 26
  if c == ' '
    then return ' '
    else return $ alphas !! index'
