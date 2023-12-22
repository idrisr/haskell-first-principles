module IdentityTest where

import Test.Tasty
import Test.Tasty.HUnit
import Identity

identityTests :: TestTree
identityTests = testGroup "Identity" [unitTests]

unitTests :: TestTree
unitTests =
    testGroup
        "unit"
        [ let
            sut :: Identity Int
            sut = Identity 15
            wot = Identity 225
            x :: Integer
            x = 2
            got = fmap (^ x) sut
           in
            testCase "" $ got @?= wot
        ]
