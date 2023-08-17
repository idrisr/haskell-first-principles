module Either where

import Prelude hiding (Either, Left, Right)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Checkers

data Either a b
    = Left a
    | Right b
    deriving (Eq, Ord, Show)

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right $ f y

instance Foldable (Either a) where
    foldMap _ (Left _) = mempty
    foldMap f (Right y) = f y
    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z

instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
    arbitrary = oneof [Left <$> arbitrary
                     , Right <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Either a b) where
    (=-=) = eq
