module Exercise where

import Control.Applicative
import Text.Trifecta
import Text.Fractions (virtuousFraction)

type FractionOrDecimal =
  Either Rational Integer

parseFod :: Parser FractionOrDecimal
parseFod =
      try (Left  <$> virtuousFraction)
  <|> (Right <$> decimal)

main = do
  print $ parseString parseFod mempty "123"
  print $ parseString parseFod mempty "1/2"
