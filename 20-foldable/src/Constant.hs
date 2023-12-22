module Constant where

import Test.Tasty.QuickCheck

newtype Constant a b = Constant b
    deriving (Show, Eq)

instance Foldable (Constant a) where
    foldr f b1 (Constant b) = f b b1

instance (Arbitrary b) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary
