module List where

import Test.Tasty.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data List a = Nil | Cons a (List a)
    deriving (Eq, Ord, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a la) = Cons (f a) $ f <$> la

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons a la) = f a <> foldMap f la
    foldr _ z Nil = z
    foldr f z (Cons a la) = f a $ foldr f z la

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons a la) = Cons <$> f a <*> traverse f la

instance Arbitrary a => Arbitrary (List a) where
    arbitrary =
        frequency
            [ (2, return Nil)
            , (1, liftA2 Cons arbitrary arbitrary)
            ]

instance Eq a => EqProp (List a) where
    (=-=) = eq
