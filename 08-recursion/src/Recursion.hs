module Recursion where

sUm :: (Num a, Eq a) => a -> a
sUm 0 = 0
sUm n = n + sUm (n-1)

mult :: Integral a => a -> a -> a
mult _ 0 = 0
mult 0 _ = 0
mult a b | b > 0 = a + mult a (b-1)
         | otherwise = a
