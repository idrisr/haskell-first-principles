module Main where

import Fraction
import Text.Trifecta

main :: IO ()
main = do
    let parseFraction2 = parseString virtuousFraction mempty
    print $ parseFraction2 badFraction
    print $ parseFraction2 shouldWork
    print $ parseFraction2 shouldAlsoWork
    print $ parseFraction2 alsoBad
