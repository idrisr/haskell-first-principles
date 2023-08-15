module Instances where

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure  = Identity
    (Identity f) <*> (Identity x)= Identity $ f x

newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap = undefined

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant a) <*> (Constant b) = Constant $ a <> b
