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
    fmap f (Node l a r) = Node l1 c r2
      where
        l1 = fmap f l
        c = f a
        r2 = fmap f r

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node l a r) = l1 <> c <> r1
      where
        l1 = foldMap f l
        c = f a
        r1 = foldMap f r

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node l a r) = liftA3 Node l1 c r1
      where
        l1 = traverse f l
        r1 = traverse f r
        c = f a

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary =
        frequency
            [ (5, return Empty)
            , (1, Leaf <$> arbitrary)
            , (1, liftA3 Node arbitrary arbitrary arbitrary)
            ]

instance Eq a => EqProp (Tree a) where
    (=-=) = eq
