module BahEither where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data BahEither b a
    = PLeft a
    | PRight b
    deriving (Show, Eq)

instance Functor (BahEither a) where
    fmap f (PLeft a) = PLeft $ f a
    fmap _ (PRight b) = PRight b

instance Monad (BahEither a ) where
    PLeft a >>= f = f a
    PRight a >>= _ = PRight a

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither a b) where
    arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]

instance (Eq a, Eq b) => EqProp (BahEither a b) where
    (=-=) = eq

instance Applicative (BahEither a) where
    pure = PLeft
    PRight f <*> _ = PRight f
    PLeft f <*> x = fmap f x
