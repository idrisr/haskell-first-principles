module First where

import Optional
import Test.QuickCheck

newtype First a = First { getFirst :: Optional a }
    deriving (Eq, Show)

instance Semigroup (First a) where
    (First Nada) <> a = a
    (First a) <> _ = First a

instance Monoid (First a) where
    mempty = First Nada

firstMappend :: First a -> First a -> First a
firstMappend = mappend

instance Arbitrary a => Arbitrary (First a) where
    arbitrary = do
        a <- arbitrary
        frequency
            [ (1, return (First Nada))
            , (4, return (First (Only a)))
            ]
