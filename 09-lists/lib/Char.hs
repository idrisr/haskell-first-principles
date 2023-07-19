module Char where

import Data.Char

a :: String -> String
a = filter isUpper

capFirst :: String -> String
capFirst [] = []
capFirst (x:xs)  = toUpper x : xs

capAll :: String -> String
capAll = map toUpper

capHead :: String -> Char
capHead = toUpper . head
