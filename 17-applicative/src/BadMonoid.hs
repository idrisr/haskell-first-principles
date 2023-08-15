module BadMonoid where

import Test.QuickCheck.Checkers
import Test.Tasty.QuickCheck

data Bull = Fools | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = oneof [return Fools, return Twoo]

instance Semigroup Bull where
    a <> Fools = a
    Fools <> a = a
    _ <> _ = Twoo

instance Monoid Bull where
    mempty = Fools

-- EqProp is from the checkers library
instance EqProp Bull where
    (=-=) = eq
