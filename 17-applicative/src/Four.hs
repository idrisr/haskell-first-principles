module Four where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (Four a b c d) <*> (Four a1 b1 c1 d1) = Four a2 b2 c2 d2
        where a2 = a <> a1
              b2 = b <> b1
              c2 = c <> c1
              d2 = d d1

instance
    (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d)
    where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq
