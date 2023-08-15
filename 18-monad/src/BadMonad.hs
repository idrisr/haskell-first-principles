module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative
import Control.Monad (join)

data CountMe a = CountMe Integer a
    deriving (Eq, Show)

instance Functor CountMe where
    fmap f (CountMe i a) = CountMe i $ f a

instance Applicative CountMe where
    pure = CountMe 0
    CountMe n f <*> CountMe n1 a = CountMe (n + n1) $ f a

instance Monad CountMe where
    return = pure
    CountMe n a >>= f = CountMe (n + n1) b
      where
        CountMe n1 b = f a

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = liftA2 CountMe arbitrary arbitrary

instance Eq a => EqProp (CountMe a) where
    (=-=) = eq

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
{- hlint ignore "Use =<<" -}
mcomp f g a = join $ f <$> g a

mcomp1 :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp1 f g a = g a >>= f
