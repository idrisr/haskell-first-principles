module Try where

import Text.Trifecta
import Text.Fraction
import Data.Ratio
import Control.Applicative

parseDecimal :: Parser Rational
parseDecimal = do
    a <- some decimal
    _ <- char '.'
    b <- some decimal
    _ <- eof
    let x :: Integer
        x = read $ foldMap show a
    let y :: Integer
        y = read $ foldMap show b
    return $ x % y

p2 :: Parser Rational
p2 = parseDecimal <|> parseFraction

p3 :: Parser Rational
p3 = try parseDecimal <|> parseFraction
