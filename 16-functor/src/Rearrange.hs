{-# LANGUAGE FlexibleInstances #-}

module Rearrange where

import Test.QuickCheck

data Sum a b
    = First a
    | Second b

instance Functor (Sum e) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second $ f b

data Company a b c
    = DeepBlue a c
    | Something b

instance Functor (Company e e') where
    fmap _ (Something b) = Something b
    fmap f (DeepBlue a c) = DeepBlue a $ f c

data More a b
    = L a b a
    | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L a (f b) a'
    fmap f (R b a b') = R (f b) a (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (More a b) where
    arbitrary =
        oneof
            [ L <$> arbitrary <*> arbitrary <*> arbitrary
            , R <$> arbitrary <*> arbitrary <*> arbitrary
            ]

data Quant a b
    = Finance
    | Desk a
    | Bloor b
    deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary =
        oneof
            [ return Finance
            , Desk <$> arbitrary
            , Bloor <$> arbitrary
            ]

{- hlint ignore "Use newtype instead of data" -}
data K a b = K a
    deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K a) = K a

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
    arbitrary = K <$> arbitrary

data EvilGoateeConst a b = GoatyConst b
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
    arbitrary = GoatyConst <$> arbitrary

data LiftItOut f a = LiftItOut (f a)
    deriving (Eq, Show)

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
    arbitrary = LiftItOut <$> arbitrary

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

data Parappa f g a = DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
    arbitrary = DaWrappa <$> arbitrary <*> arbitrary

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) $ fmap f ga

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa ga) = IgnoringSomething fa $ fmap f ga

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
    arbitrary = IgnoringSomething <$> arbitrary <*> arbitrary

data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t) ) => Arbitrary (Notorious g o a t) where
    arbitrary = Notorious <$> arbitrary <*> arbitrary <*> arbitrary

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary =
        frequency
            [ (20, Cons <$> arbitrary <*> arbitrary)
            , (1, return Nil)
            ]

data GoatLord a
    = NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat $ f a
    fmap f (MoreGoats a b c) = MoreGoats x y z
      where
        x = fmap f a
        y = fmap f b
        z = fmap f c

instance Arbitrary a => Arbitrary (GoatLord a) where
    arbitrary =
        frequency
            [ (1, OneGoat <$> arbitrary)
            , (5, return NoGoat)
            , (1, MoreGoats <$> arbitrary <*> arbitrary <*> arbitrary)
            ]
