module Pair where

import Test.Tasty.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data Pair a b = Pair a b
    deriving (Eq, Ord, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
    foldMap f (Pair _ b) = f b
    foldr f z (Pair _ b) = f b z

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq
