module Algebra where

import Data.Int (Int8)

data Ex0 = Ex0
    deriving (Eq, Show)

--unary
{- HLINT ignore "Use newtype instead of data" -}
data Ex1 = Ex1 Int
    deriving (Eq, Show)

-- product
data Ex2 = Ex2 Int String
    deriving (Eq, Show)

-- cardinality
x = minBound :: Int8
y = maxBound :: Int8

data BigSmall = Big
        | Small
    deriving (Eq, Show, Enum)

data NumberOrBool =
    Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)
