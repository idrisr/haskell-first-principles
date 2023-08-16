module Maybe where

import Data.Monoid
import Data.Foldable

aa :: Integer
aa = foldr (+) 1 Nothing

fm :: Maybe (Sum Integer) -> Sum Integer
fm = foldMap (+1)

b :: Sum Integer
b = fm Nothing :: Sum Integer

data Optional a = Nada | Yep a

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep x) = f x z
    foldl _ z Nada = z
    foldl f z (Yep x) = f z x
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

c :: Foldable t => t a -> [a]
c = toList

d :: [Int]
d = toList (Just 1)

e :: [Maybe Integer]
e = [Just 1, Just 2, Just 3]

ff :: [[Integer]]
ff = map toList e
