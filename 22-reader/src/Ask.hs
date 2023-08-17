{-# LANGUAGE InstanceSigs #-}

module Ask where

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
    fmap f (Reader r) = Reader $ f . r

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader a) <*> (Reader b) = Reader $ \r -> a r $ b r

instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader x) >>= y = Reader $ \r -> runReader (y $ x r) r
