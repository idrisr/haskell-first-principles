module Comprehension where

b :: Integer
b = 2
mySqr :: [Integer]
mySqr = [x^b | x <- [1..10]]
a = [x | x <- mySqr, rem x 2 == 10]
