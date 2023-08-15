module ZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype ZipList' a = ZipList' [a]
    deriving (Eq, Show)

getList :: ZipList' a -> [a]
getList (ZipList' a) = a

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
      where
        xs' =
            let (ZipList' l) = xs
             in take 3000 l
        ys' =
            let (ZipList' l) = ys
             in take 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure x = ZipList' $ repeat x
    ZipList' [] <*> _ = ZipList' []
    _ <*> ZipList' [] = ZipList' []
    ZipList' (f : fs) <*> ZipList' (x : xs) = ZipList' $ y : ys
      where
        y = f x
        ys = getList $ ZipList' fs <*> ZipList' xs

instance Monoid a => Semigroup (ZipList' a) where
    ZipList' [] <> _ = ZipList' []
    _ <> ZipList' [] = ZipList' []
    ZipList' (x : xs) <> ZipList' (y : ys) = ZipList' $ z : zs
      where
        z = x <> y
        zs = getList $ ZipList' xs <> ZipList' ys

instance Monoid a => Monoid (ZipList' a) where
    mempty = ZipList' $ repeat mempty

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary
