module Identity where

newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a
