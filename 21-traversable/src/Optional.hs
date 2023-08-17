module Optional where

import Test.Tasty.QuickCheck
import Test.QuickCheck.Checkers

data Optional a = Nada | Yep a
    deriving (Eq, Ord, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a
    foldr _ z Nada = z
    foldr f z (Yep a) = f a z

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = oneof [return Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq
