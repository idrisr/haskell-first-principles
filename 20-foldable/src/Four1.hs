module Four1 where

import Test.Tasty.QuickCheck

data Four1 a b = Four1 a b b b
    deriving (Show, Eq)

instance Foldable (Four1 a) where
    foldr f a (Four1 _ _ _ b) = f b a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four1 a b) where
    arbitrary = Four1 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
