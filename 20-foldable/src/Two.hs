module Two where

import Test.Tasty.QuickCheck
import Control.Applicative

data Two a b = Two a b
    deriving (Show, Eq)

instance Foldable (Two a) where
    foldr f a (Two _ b) = f b a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = liftA2 Two arbitrary arbitrary
