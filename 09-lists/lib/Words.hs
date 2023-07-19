module Words where

import Data.Char

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

breaker :: Char -> String -> [String]
breaker _ [] = []
breaker c (x:xs) | x == c = breaker c xs
breaker c x = a : breaker c b
        where a = takeWhile f x
              b = dropWhile f x
              f = (/=c)

myWords :: String -> [String]
myWords = breaker ' '

myLines :: String -> [String]
myLines = breaker '\n'
