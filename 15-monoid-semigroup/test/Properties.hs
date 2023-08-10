module Properties where

propSemiAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
propSemiAssoc a b c = a <> (b <> c) == (a <> b) <> c

propMonoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
{- hlint ignore "Monoid law, left identity" -}
propMonoidLeftIdentity a = (mempty <> a) == a

propMonoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
{- hlint ignore "Monoid law, right identity" -}
propMonoidRightIdentity a = (a <> mempty) == a
