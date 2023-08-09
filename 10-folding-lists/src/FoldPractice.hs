module FoldPractice where

-- variations
-- direct recursion, not using &&
-- direct recursion, using &&
-- fold, not point-free
-- fold point-free

myOr1 :: [Bool] -> Bool
myOr1 [] = False
{- hlint ignore "Redundant if" -}
{- hlint ignore "Redundant ==" -}
myOr1 (x:xs) = if x == True then True else myOr1 xs

myOr2 :: [Bool] -> Bool
{- hlint ignore "Use foldr" -}
myOr2 [] = False
myOr2 (x:xs) = x || myOr2 xs

myOr3 :: [Bool] -> Bool
{- hlint ignore "Avoid lambda" -}
myOr3 = foldr (\a b -> a || b) False

myOr4 :: [Bool] -> Bool
{- hlint ignore "Use or" -}
myOr4 = foldr (||) False

myAny1 :: (a -> Bool) -> [a] -> Bool
myAny1 _ [] = False
myAny1 f (x:xs) = if f x then True else myAny1 f xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 _ [] = False
myAny2 f (x:xs) = f x || myAny2 f xs

myAny3 :: (a -> Bool) -> [a] -> Bool
{- hlint ignore "Eta reduce" -}
myAny3 f xs = foldr (\e a -> f e || a) False xs

myAny4 :: (a -> Bool) -> [a] -> Bool
myAny4 f = foldr (\e a -> f e || a) False

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 _ [] = False
myElem1 x (y:ys) = if x==y then True else myElem1 x ys

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 _ [] = False
myElem2 x (y:ys) = x==y || myElem1 x ys

myElem3 :: Eq a => a -> [a] -> Bool
myElem3 x ys = foldr (\e a -> e==x || a) False ys

myElem4 :: Eq a => a -> [a] -> Bool
myElem4 x = foldr (\e a -> e==x || a) False

myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 (x:xs) = myReverse1 xs ++ [x]

myReverse2 :: [a] -> [a]
myReverse2 xs = foldr (\e a -> a ++ [e]) [] xs

myReverse3 :: [a] -> [a]
myReverse3 = foldr (\e a -> a ++ [e]) []
