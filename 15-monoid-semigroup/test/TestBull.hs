module TestBull where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Properties
import Bull

bullTests :: TestTree
bullTests  = testGroup "bull monoid" [assocTest, identityTest]

type BullMappend = Bull -> Bull -> Bull -> Bool

assocTest :: TestTree
assocTest =
    testGroup
        "associativity"
        [QC.testProperty "bull" (propSemiAssoc :: BullMappend)]

identityTest :: TestTree
identityTest =
    testGroup
        "identity"
        [ QC.testProperty "left" (propMonoidLeftIdentity :: Bull -> Bool)
        , QC.testProperty "right" (propMonoidRightIdentity :: Bull -> Bool)
        ]
