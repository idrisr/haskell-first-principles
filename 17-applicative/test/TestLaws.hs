module TestLaws where

import Test.QuickCheck

data Bull = Fools | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
    _ <> _ = Fools

instance Monoid Bull where
    mempty = Fools
