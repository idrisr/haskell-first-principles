module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord _ = undefined

digits :: Int -> [Int]
digits _ = undefined

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
