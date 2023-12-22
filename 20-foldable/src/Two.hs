module Two where

import Test.Tasty.QuickCheck
import Control.Applicative

data Two a b = Two a b
    deriving (Show, Eq)

instance Foldable (Two a) where
    foldr f b1 (Two _ b) = f b b1

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = liftA2 Two arbitrary arbitrary
