module Validation where

import qualified Test.QuickCheck as QC
import Test.QuickCheck.Checkers

data Validation e a
    = Failure e
    | Success a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Success a) = Success $ f a
    fmap _ (Failure b) = Failure b

instance Monoid e => Applicative (Validation e) where
    pure = Success
    Failure e <*> Failure f = Failure $ e <> f
    Success f <*> Success x = Success $ f x
    Failure f <*> _ = Failure f
    _ <*> Failure f = Failure f

instance (QC.Arbitrary e, QC.Arbitrary a) => QC.Arbitrary (Validation e a) where
    arbitrary = QC.oneof [Success <$> QC.arbitrary, Failure <$> QC.arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq
