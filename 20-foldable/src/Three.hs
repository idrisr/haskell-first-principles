module Three where

import Test.Tasty.QuickCheck
import Control.Applicative

data Three a b c = Three a b c
    deriving (Show, Eq)

instance Foldable (Three a b) where
    foldr f a (Three _ _ b) = f b a

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary
