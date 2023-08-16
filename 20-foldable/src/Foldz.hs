module Foldz where

import Data.Monoid
import Data.Foldable

a :: Sum Integer
{- hlint ignore "Use foldMap" -}
a = fold $ fmap Sum [1 .. 5]

b :: Product Integer
b = fold $ fmap Product [1 .. 5]

c :: Sum Integer
c = foldMap Sum [1, 2, 3, 4]

d :: Product Integer
d = foldMap Product [1, 2, 3, 4]

e :: All
e = foldMap All [True, True, False]

ff :: Any
ff = foldMap Any [True, True, False]

g :: Any
g = foldMap Any [True, True, False]

h :: First Integer
h = foldMap First [Just 1, Nothing, Just 5]

i :: Last Integer
i = foldMap Last [Just 1, Nothing, Just 5]

j :: Product Integer
j = foldMap f [1 .. 5]
  where
    f = (* 5) . Product

k :: Sum Integer
k = foldMap f [1 .. 5]
  where
    f = (* 5) . Sum
