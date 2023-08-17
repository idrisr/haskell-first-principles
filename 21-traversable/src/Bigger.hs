module Bigger where

import Test.Tasty.QuickCheck
import Test.QuickCheck.Checkers

data Bigger a b = Bigger a b b b
    deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
    foldMap f (Bigger _ b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
    traverse f (Bigger a b c d) = Bigger a <$> f b <*> f c <*> f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq
