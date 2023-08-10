{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Semigroup where
import Test.QuickCheck hiding (Failure, Success)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

newtype Identity a = Identity a
    deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity $ a <> b

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

data Two a b = Two a b
    deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

data Three a b c = Three a b c
    deriving (Show, Eq)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

data Four a b c d = Four a b c d
    deriving (Show, Eq)

instance
    (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
    Semigroup (Four a b c d)
    where
    (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
    mempty = Four mempty mempty mempty mempty

instance
    (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d)
    where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

newtype BoolConj = BoolConj Bool
    deriving (Show, Eq, Arbitrary, Ord)

instance Semigroup BoolConj where
    (BoolConj a) <> (BoolConj b) = BoolConj $ a && b

instance Monoid BoolConj where
    mempty = BoolConj True

newtype BoolDisj = BoolDisj Bool
    deriving (Show, Eq, Arbitrary)

instance Semigroup BoolDisj where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj $ a || b

instance Monoid BoolDisj where
    mempty = BoolDisj False

data Or a b = Fst a | Snd b
    deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof
            [ return $ Fst a
            , return $ Snd b
            ]

instance Semigroup (Or a b) where
    (Fst _) <> (Snd b) = Snd b
    (Snd a) <> _ = Snd a
    (Fst _) <> (Fst b) = Fst b

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \a -> f a <> g a

-- Generate an arbitrary function of type a -> b
genFunction :: Arbitrary b => Gen (a -> b)
genFunction = fmap const arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = fmap Combine genFunction

data Validation a b = Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Failure a) <> (Failure b) = Failure $ a <> b
    (Success a) <> _ = Success a
    _ <> (Success a) = Success a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Failure a, return $ Success b]
