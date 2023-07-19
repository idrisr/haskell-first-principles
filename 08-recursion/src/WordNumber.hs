module WordNumber where

import Data.Char (digitToInt)
import Data.List (intercalate)

digits :: Int -> [Int]
digits = map digitToInt . show

digitToWord :: Int -> String
digitToWord n = intercalate "-" $ map wordNumber $ digits n

wordNumber :: Int -> String
wordNumber 0 = "zero"
wordNumber 1 = "one"
wordNumber 2 = "two"
wordNumber 3 = "three"
wordNumber 4 = "four"
wordNumber 5 = "five"
wordNumber 6 = "six"
wordNumber 7 = "seven"
wordNumber 8 = "eight"
wordNumber 9 = "nine"
wordNumber _ = "WOT"
