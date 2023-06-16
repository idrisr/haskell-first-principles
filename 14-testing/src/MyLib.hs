module MyLib where

import Prelude hiding (head, minimum)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

safeDiv :: Int -> Maybe Int
safeDiv x = Just x

data Color
    = Red
    | Orange
    | Fuchsia
    | Green
    | Blue
    deriving (Enum, Show)

isGoodColor :: Color -> Bool
isGoodColor Red = False
isGoodColor Orange = True
isGoodColor Fuchsia = True
isGoodColor Green = False
isGoodColor Blue = True

head :: [a] -> a
head (x : _) = x
head [] = error "Prelude.head: empty list"

minimum :: (Ord a) => [a] -> a
minimum [] = error "Prelude.minimum: empty list"
minimum xs = foldl1 min xs

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)
