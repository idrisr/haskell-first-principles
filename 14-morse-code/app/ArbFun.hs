module Main where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)
data A = B | C | D deriving (Show)

instance Arbitrary A where
    arbitrary = frequency [(1, return B),
                           (2, return C),
                           (100, return D)]

trivialGen :: Gen Trivial
trivialGen = return Trivial

newtype Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = arbitrary >>= \a -> return (Identity a)

main :: IO ()
main =
    sample trivialGen >>
    sample (identityGen::Gen (Identity Int)) >>
    sample (identityGen::Gen (Identity A))
