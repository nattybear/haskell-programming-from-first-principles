module Exercise where

import Control.Applicative
import Text.Trifecta
import Text.Fractions (parseFraction)

type FractionOrDecimal =
  Either Rational Integer

parseFod :: Parser FractionOrDecimal
parseFod =
      try (Left  <$> parseFraction)
  <|>     (Right <$> decimal)

main = do
  print $ parseString parseFod mempty "123"
  print $ parseString parseFod mempty "1/2"
