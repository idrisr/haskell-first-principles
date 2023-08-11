module Instances where

import Test.QuickCheck
newtype Identity a = Identity a

data Pair a = Pair a a
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b
    deriving (Show, Eq)

instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

data Three a b c = Three a b c
    deriving (Show, Eq)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

data Three' a b = Three' a b b
    deriving (Show, Eq)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

data Four a b c d = Four a b c d
    deriving (Show, Eq)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c $ f d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data Four' a b = Four' a a a b
    deriving (Show, Eq)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c $ f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

newtype Constant a b = Constant {getConstant :: a}
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant v) = Constant v

instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

{- HLINT ignore "Use newtype instead of data" -}
data Wrap f a = Wrap (f a)
    deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap $ fmap f fa
