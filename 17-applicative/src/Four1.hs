module Four1 where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Four1 a b = Four1 a a a b
    deriving (Eq, Show)

instance Functor (Four1 a) where
    fmap f (Four1 a b c d) = Four1 a b c $ f d

instance (Monoid a) => Applicative (Four1 a) where
    pure = Four1 mempty mempty mempty
    (Four1 a b c d) <*> (Four1 a1 b1 c1 d1) = Four1 a2 b2 c2 d2
        where a2 = a <> a1
              b2 = b <> b1
              c2 = c <> c1
              d2 = d d1

instance
    (Arbitrary a, Arbitrary b) => Arbitrary (Four1 a b) where
    arbitrary = Four1 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four1 a b) where
    (=-=) = eq
