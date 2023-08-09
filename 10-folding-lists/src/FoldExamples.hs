module FoldExamples where

xs1 :: [String]
xs1 = map show ([1 .. 10] :: [Integer])

ff :: String -> String -> String
ff a b = concat ["(", a, "+", b, ")"]

r :: String
r = foldr ff "0" xs1

l :: String
l = foldl ff "0" xs1

ys :: [Integer]
ys = [1, 2] ++ undefined

x1 :: Int
x1 = foldr (^) 2 [1..3]
x2 :: Int
x2 = foldl (^) (2::Int) ([1..3]::[Int])

x3 :: [Int]
x3 = foldr (:) [] [1..3]

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b []  = b
myFoldr f b (x:xs) = f x $ myFoldr f b xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ b []  = b
myFoldl f b (x:xs) = myFoldl f (f b x) xs

j :: [Int]
j = foldr (:) [] [1..10]

k :: [Int]
k = foldl (flip (:)) [] [1..10]
