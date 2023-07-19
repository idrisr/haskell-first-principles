module Bottom where

a = [x^y | x <- [(1::Integer)..5], y <- [2::Integer, undefined]]
b = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
c = sum [1, undefined, 3]
d = length [1, 2, undefined]
e = length $ [1, 2, 3] ++ undefined
f = take 1 $ filter even [1, 2, 3, undefined]
g = take 1 $ filter even [1, 3, undefined]
h = take 1 $ filter odd [1, 3, undefined]
i = take 1 $ map (+1) [undefined, 2, 3]
j = take 1 $ map (+1) [1, undefined, 3]
k = take 2 $ map (+1) [1, undefined, 3]
