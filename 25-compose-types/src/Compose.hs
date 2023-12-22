{-# LANGUAGE InstanceSigs #-}

module Compose where

import Test.QuickCheck.Checkers
import Test.Tasty.QuickCheck

newtype Compose f g a = Compose {getCompose :: f (g a)}
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose a) = Compose $ (fmap . fmap) f a

newtype One f a = One (f a)
    deriving (Eq, Show)

instance Functor f => Functor (One f) where
    fmap f (One a) = One $ f <$> a

newtype Three f g h a = Three (f (g (h a)))
    deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
    fmap f (Three a) = Three $ (fmap . fmap . fmap) f a

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ pure (pure a)

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    Compose f <*> Compose a = Compose $ (<*>) <$> f <*> a

instance (Monad f, Monad g) => Monad (Compose f g) where
    return = pure

    (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
    (>>=) = undefined

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose xs) = foldMap (foldMap f) xs

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose fg) = Compose <$> traverse (traverse f) fg

instance (Arbitrary (f (g a))) => Arbitrary (Compose f g a) where
    arbitrary = Compose <$> arbitrary

instance Eq (f (g a)) => EqProp (Compose f g a) where
    (=-=) = eq
