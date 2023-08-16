module Identity where

import Data.Monoid

newtype Identity a = Identity a

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

a = foldr (*) 1 (Identity 5)
b = foldl (*) 5 (Identity 5)
c = foldMap (*5)
type PI = Product Integer
d = c (Identity 100) :: PI
