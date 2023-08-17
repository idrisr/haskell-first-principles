module ReaderPractice where

-- import Control.Applicative
import Data.Maybe

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

z1 :: Integer -> Maybe Integer
z1 n = lookup n $ zip x z


x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- x3
    -- takes one input and
    -- makes a tuple of the results
    -- of two applications of z1
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z1 <*> z1

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s1 :: Maybe Integer
s1 = summed <$> ((,) <$> xs <*> ys)

q1 :: Bool
q1 = foldr (&&) True $ sequA 11

q2 :: Integer
q2 = fromMaybe 0 s1

q3 :: Bool
q3 = bolt $ fromMaybe 0 ys
