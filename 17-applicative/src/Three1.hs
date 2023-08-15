module Three1 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data Three1 a b = Three1 a b b
    deriving (Eq, Show)

instance Functor (Three1 a) where
    fmap f (Three1 a b c) = Three1 a (f b) (f c)

instance Monoid a => Applicative (Three1 a) where
    pure x = Three1 mempty x x
    (Three1 a b c) <*> (Three1 a1 b1 c1) = Three1 a2 b2 c2
      where
        a2 = a <> a1
        b2 = b b1
        c2 = c c1

instance (Arbitrary a,  Arbitrary b) => Arbitrary (Three1 a b) where
    arbitrary = liftA3 Three1 arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three1 a b) where
    (=-=) = eq
