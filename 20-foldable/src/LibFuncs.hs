module LibFuncs where

sum :: (Num a, Foldable t) => t a -> a
sum = foldr (+) 0

product :: (Num a, Foldable t) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\a b -> (a == x) || b) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr f Nothing
    where f a (Just b) = Just $ min a b
          f a Nothing = Just a

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing
    where f a (Just b) = Just $ max a b
          f a Nothing = Just a

null :: Foldable t => t a -> Bool
null = foldr (const . const False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ b -> b + 1) 0

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

-- Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap (mempty <>)

-- 10. Define foldMap in terms of foldr:
foldMoop :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMoop f = foldr (\a b -> f a <> b) mempty
