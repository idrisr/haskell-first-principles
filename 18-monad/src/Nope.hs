module Nope where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Nope a = NopeDotJpg
    deriving (Show, Eq)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    _ >>= _ = NopeDotJpg

instance Arbitrary a =>  Arbitrary (Nope a) where
    arbitrary = pure NopeDotJpg

instance Eq a => EqProp (Nope a) where
    (=-=) = eq
