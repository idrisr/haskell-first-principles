module List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary =
        frequency
            [ (1, return Nil)
            , (1, liftA2 Cons arbitrary arbitrary)
            ]

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a la) = Cons (f a) $ fmap f la

instance Semigroup (List a) where
    Nil <> a = a
    a <> Nil = a
    (Cons a Nil) <> b = Cons a b
    (Cons a la) <> b = Cons a $ la <> b

instance Monoid (List a) where
    mempty = Nil

-- Writing the list Applicative is similar:
instance Applicative List where
    pure = flip Cons Nil
    Nil <*> _ = Nil
    (Cons f lf) <*> b = fmap f b <> (lf <*> b)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as
