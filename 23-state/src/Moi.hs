{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Moi where
import Test.Tasty.QuickCheck

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \s ->
        let (a, s1) = g s
         in (f a, s1)

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi (a,)
    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ \s ->
        let (h, s1) = f s
            (a, s2) = g s1
         in (h a, s2)

instance Monad (Moi s) where
    return = pure
    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ \s ->
        let (a, s1) = f s
         in runMoi (g a) s1

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Moi s a) where
    arbitrary = Moi <$> arbitrary
