module Division (dividedBy, DividedResult( Result, DivideByZero)) where

import Prelude hiding (negate)

data DividedResult a = Result a | DivideByZero
    deriving (Show, Eq)

negate :: Num a => DividedResult a -> DividedResult a
negate DivideByZero = DivideByZero
negate (Result a) = Result (-a)

dividedBy :: (Integral a) => a -> a -> DividedResult a
dividedBy _ 0 = DivideByZero
dividedBy n d | n > 0 && d < 0 = negate $ dividedBy n (-d)
dividedBy n d | n < 0 && d > 0 = negate $ dividedBy (-n) d
dividedBy n d | n < 0 && d < 0 = dividedBy (-n) (-d)
dividedBy n d = Result $ go n d 0
  where
    go a b c
        | a < b = c
        | otherwise = go (a - b) b c + 1
