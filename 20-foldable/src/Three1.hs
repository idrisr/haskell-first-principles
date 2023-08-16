module Three1 where

import Test.Tasty.QuickCheck
import Control.Applicative

data Three1 a b = Three1 a b b
    deriving (Show, Eq)

instance Foldable (Three1 a) where
    foldr f a (Three1 _ _ b) = f b a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three1 a b) where
    arbitrary = liftA3 Three1 arbitrary arbitrary arbitrary
