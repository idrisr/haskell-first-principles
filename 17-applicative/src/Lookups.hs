module Lookups where

import Data.List (elemIndex)

-- 1. pure
-- 2. (<$>)
-- 3. (<*>)

-- Make the following expressions type check:
added :: Maybe Integer
added = (+3) <$> lookup (3::Integer) (zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup (3::Integer) (zip [1, 2, 3] [4, 5, 6])

z :: Maybe Integer
z = lookup (2::Int) (zip [1, 2, 3] [4, 5, 6])

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex (3::Int) [1, 2, 3, 4, 5]

y2 :: Maybe Int
y2 = elemIndex (4::Int) [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y2

xs :: [Integer]
xs = [1, 2, 3]
ys :: [Integer]
ys = [4, 5, 6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys
