import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case palindrome' line1 of
    True  -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

palindrome' :: String -> Bool
palindrome' s = s' == reverse s'
  where s' = filter (`elem` ['a'..'z']) (map toLower s)
