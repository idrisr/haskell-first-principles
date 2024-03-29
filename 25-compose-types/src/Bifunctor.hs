module Bifunctor where

import Prelude hiding (Either, Left, Right)

data Deux a b = Deux a b
newtype Const a b = Const a
data Drei a b c = Drei a b c
data SuperDrei a b c = SuperDrei a b
newtype SemiDrei a b c = SemiDrei a
data Quadriceps a b c d = Quadzzz a b c d
data Either a b = Left a | Right b

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    {- hlint ignore "Use bimap" -}
    bimap f g = first f . second g
    first :: (a -> b) -> p a c -> p b c
    {- hlint ignore "Use first" -}
    first f = bimap f id
    second :: (b -> c) -> p a b -> p a c
    second = bimap id

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

instance Bifunctor Const where
    bimap f _ (Const a) = Const $ f a

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

instance Bifunctor Either where
    bimap f _ (Left a) = Left $ f a
    bimap _ g (Right b) = Right $ g b
