module Big where

import Test.Tasty.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative

data Big a b = Big a b b
    deriving (Eq, Ord, Show)

instance Functor (Big a) where
    fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
    foldMap f (Big _ b c) = f b <> f c

instance Traversable (Big a) where
    traverse f (Big a b c) = Big a <$> f b <*> f c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq

-- more than one value of type b
--
-- Foldable
    -- use Monoid

-- Traversable
    -- use Applicative
