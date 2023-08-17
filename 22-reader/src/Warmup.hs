module Warmup where

import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled1 :: [Char] -> ([Char], [Char])
tupled1 = cap >>=
    \a -> rev >>=
    \b -> return (a, b)

tupled2 :: [Char] -> ([Char], [Char])
tupled2 = do
    a <- cap
    b <- rev
    return (a, b)
