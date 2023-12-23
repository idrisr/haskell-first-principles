module Tree where

import Control.Applicative
import Test.QuickCheck.Checkers
import Test.Tasty.QuickCheck

data Tree a
    = Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node t1 a t2) = Node (f <$> t1) (f a) (f <$> t2)

instance Foldable Tree where
    foldr _ b Empty = b
    foldr f b (Leaf a) = f a b
    foldr f b (Node t1 a t2) = foldr f (f a (foldr f b t1)) t2

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node t1 a t2) = Node <$> traverse f t1 <*> f a <*> traverse f t2

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary =
        frequency
            [ (5, return Empty)
            , (1, Leaf <$> arbitrary)
            , (1, liftA3 Node arbitrary arbitrary arbitrary)
            ]

instance Eq a => EqProp (Tree a) where
    (=-=) = eq
