module StdFunc where

myAnd :: [Bool] -> Bool
{- hlint ignore "Use foldr" -}
myAnd [] = True
myAnd (x : xs) = x && myAnd xs

myOr :: [Bool] -> Bool
{- hlint ignore "Use foldr" -}
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (x ==)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squish2 :: [[a]] -> [a]
squish2 = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f (x : y : xs) =
    case f x y of
        GT -> myMaximumBy f (x : xs)
        EQ -> myMaximumBy f (x : xs)
        LT -> myMaximumBy f (y : xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy _ [x] = x
myMinimumBy f (x : y : xs) =
    case f x y of
        GT -> myMinimumBy f (y : xs)
        EQ -> myMinimumBy f (x : xs)
        LT -> myMinimumBy f (x : xs)

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy $ \a b -> if a > b then GT else LT

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy $ \a b -> if a > b then LT else GT
