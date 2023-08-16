module LibFuncs where

sum :: (Monoid a, Foldable t) => t a -> a
sum = foldMap (<> mempty)

product :: (Monoid a, Foldable t) => t a -> a
product = foldMap (<> mempty)

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\e a -> x == e || a) False

extreme :: (Foldable t, Ord a) => (a -> a -> a) -> t a -> Maybe a
extreme f = foldr g Nothing
  where
    g e a = case (e, a) of
        (e1, Nothing) -> Just e1
        (e1, Just a1) -> Just $ f e1 a1

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = extreme min

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = extreme max

null :: Foldable t => t a -> Bool
null = let f _ _ = False in foldr f True

length :: (Foldable t) => t a -> Int
length = foldr (\_ e -> e + 1) 0

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

-- Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap (<> mempty)

-- 10. Define foldMap in terms of foldr:
foldMop :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMop f = flip foldr mempty $ \a e -> f a <> e
