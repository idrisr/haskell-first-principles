{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Newtype where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Integer where
    tooMany n = n > 42

newtype Goats = Goats Int
    deriving (Eq, Show)

newtype Cows = Cows Int
    deriving (Eq, Show, TooMany)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

instance TooMany ((, ) Int String) where
    tooMany (n, _) = n > 42

instance (Num a, TooMany a, Ord a) => TooMany (a, a) where
    tooMany (n, m) = n+m > 42

instance TooMany ((, ) Int Int) where
    tooMany (n, m) = n+m > 42

x = tooMany ((3, "YO")::(Int, String))
