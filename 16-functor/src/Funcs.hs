module Funcs where

import Test.QuickCheck

data WhoCares a = ItDoesnt
    | Matter a
    | WhatThisIsCalled
    deriving (Eq, Show)

instance Functor WhoCares where
    fmap _ ItDoesnt = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled
    fmap f (Matter a) = Matter (f a)

instance Arbitrary a => Arbitrary (WhoCares a) where
    arbitrary =
        oneof
            [ return ItDoesnt
            , return WhatThisIsCalled
            , Matter <$> arbitrary
            ]
