module List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data List a
    = Nil
    | Cons a (List a)
    deriving (Eq, Ord, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons y ys
      where
        y = f x
        ys = fmap f xs

instance Semigroup (List a) where
    Nil <> a = a
    a <> Nil = a
    Cons a Nil <> b = Cons a b
    Cons a la <> b = Cons a $ la <> b

instance Monoid (List a) where
    mempty = Nil

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> xs = y <> ys
      where
        y = f <$> xs
        ys = fs <*> xs

instance Monad List where
    Nil >>= _ = Nil
    Cons a la >>= f = x <> xs
      where
        x = f a
        xs = la >>= f

instance Arbitrary a => Arbitrary (List a) where
    arbitrary =
        frequency
            [ (1, return Nil)
            , (3, liftA2 Cons arbitrary arbitrary)
            ]

instance Eq a => EqProp (List a) where
    (=-=) = eq
