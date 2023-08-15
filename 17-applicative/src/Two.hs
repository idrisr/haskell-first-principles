module Two where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (Two a g) <*> (Two a1 y) = Two (a <> a1) $ g y

instance (Arbitrary a,  Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq
