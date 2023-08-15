module Three where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (Three a b c) <*> (Three a1 b1 c1) = Three (a <> a1) (b <> b1) $ c c1

instance (Arbitrary a,  Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq
