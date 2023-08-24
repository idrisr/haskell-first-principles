{-# LANGUAGE OverloadedStrings #-}

module Text.Fraction where

import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (MonadFail m, TokenParsing m) => m Rational
parseFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

parseInt :: Parser Integer
parseInt = do
    i <- integer
    _ <- eof
    return i

parseInt1 :: Parser Integer
parseInt1 = integer >>= \i -> eof >> return i

mymain :: IO ()
mymain = do
    let attoP = parseOnly parseFraction
    print $ attoP badFraction
    print $ attoP shouldWork
    print $ attoP shouldAlsoWork
    print $ attoP alsoBad

    let p = parseString parseFraction mempty
    print $ p badFraction
    print $ p shouldWork
    print $ p shouldAlsoWork
    print $ p alsoBad
