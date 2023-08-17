{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.Tasty.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data S n a = S (n a) a
    deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a)
    where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a)
    where
    (=-=) = eq

instance Functor n => Functor (S n) where
    fmap f (S n a) = S (fmap f n) $ f a

instance Foldable n => Foldable (S n) where
    foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
    traverse f (S n a) = liftA2 S n1 b
        where n1 = traverse f n
              b = f a
