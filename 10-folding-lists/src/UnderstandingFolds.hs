module UnderstandingFolds where

q1 :: Integer
{- hlint ignore "Use product" -}
q1 = foldr (*) 1 [1..5]

-- [1..5]
-- 1:2:3:4:5:[]
-- (1 `f` (2 `f` (3 `f` (4 `f` (5 `f` 1)))))

-- q1a = flip (*) 1 [1..5]
q1b = foldl (flip (*)) 1 [1..5]
q1c = foldl (*) 1 [1..5]

q2 :: Int
q2 = foldl (flip (*)) 1 [1..3]
-- 1:2:3:[]
-- (1 `f` 2) `f` 3) `f` [])
-- (1 `f` 2) `f` 3) `f` 1)
--

q5a =  foldr (++) "" ["woot", "WOOT", "woot"]
q5b = foldr max [] ["fear",  "is",  "the", "little", "death"]
q5c = foldr (&&) True [False, True]
q5d = foldr (||) True [False, False]
q5e = foldl (++) "" $ map show [1..5]
q5f = foldr (flip const) 'a' [1..5]
q5g = foldr (const id) 0 "tacos"
q5h = foldl const 0 "burritos"

q6i :: String
q6i = foldl (\b a -> b ++ show a) "z" [1..(5::Integer)]

-- foldl (flip const) 'z' [1..5]
-- 1:2:3:4:5:[]
-- ((((('z' f 1) f 2) f 3) f 4) f 5)
-- ((((('z'f 1) f 2) f 3) f 4) f 5)

-- q6i = foldl (\b a -> b ++ show a) "z" [1..(5::Integer)]
-- 1:2:3:4:5:[]
-- ("z" f 1) f 2) f 3) f 4) f 5)
-- z12345
