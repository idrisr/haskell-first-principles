module IdentityT where

newtype Identity a = Identity {runIdentity :: a}
    deriving (Eq, Show)

newtype IdentityT f a = IdentityT {runIdentityT :: f a}
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Functor m => Functor (IdentityT m) where
    fmap f (IdentityT a) = IdentityT $ fmap f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

instance Applicative m => Applicative (IdentityT m) where
    pure x = IdentityT $ pure x
    (IdentityT f) <*> (IdentityT a) = IdentityT $ f <*> a

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

instance Monad m => Monad (IdentityT m) where
    return = pure
    (IdentityT a) >>= f = IdentityT $ a >>= runIdentityT . f
