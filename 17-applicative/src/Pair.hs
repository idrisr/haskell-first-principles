module Pair where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = liftA2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where
    (=-=) = eq
