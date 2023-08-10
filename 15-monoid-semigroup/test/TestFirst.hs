module TestFirst where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Properties
import First

type FirstMappend = First String -> First String -> First String -> Bool
type FstId = First String -> Bool

firstTests :: TestTree
firstTests  = testGroup "first monoid" [assocTest, identityTest]

assocTest :: TestTree
assocTest =
    testGroup
        "associativity"
        [QC.testProperty "first" (propSemiAssoc :: FirstMappend)]

identityTest :: TestTree
identityTest =
    testGroup
        "identity"
        [ QC.testProperty "left" (propMonoidLeftIdentity :: FstId)
        , QC.testProperty "right" (propMonoidRightIdentity :: FstId)
        ]
