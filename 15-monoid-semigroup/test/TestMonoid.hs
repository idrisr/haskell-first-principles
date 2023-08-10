module TestMonoid where

import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding (Failure, Success)
import Properties
import Semigroup
import Data.Monoid

type TriId = Trivial -> Bool
type IdId = Identity String -> Bool
type TwoId = Two String (Sum Int) -> Bool
type ThreeId = Three String (Sum Int) (Product Integer) -> Bool
type FourId = Four String (Sum Int) (Product Integer) (Product Int)-> Bool
type BoolConjId = BoolConj -> Bool
type BoolDisjId = BoolDisj -> Bool

monoidTests :: TestTree
monoidTests  = testGroup "monoid2" [identityTest]

identityTest :: TestTree
identityTest =
    testGroup
        "identity"
        [ QC.testProperty "trivial left" (propMonoidLeftIdentity :: TriId)
        , QC.testProperty "trivial right" (propMonoidRightIdentity :: TriId)
        , QC.testProperty "identity left" (propMonoidLeftIdentity :: IdId)
        , QC.testProperty "identity right" (propMonoidRightIdentity :: IdId)
        , QC.testProperty "two left" (propMonoidLeftIdentity :: TwoId)
        , QC.testProperty "two right" (propMonoidRightIdentity :: TwoId)
        , QC.testProperty "Three left" (propMonoidLeftIdentity :: ThreeId)
        , QC.testProperty "Three right" (propMonoidRightIdentity :: ThreeId)
        , QC.testProperty "Four left" (propMonoidLeftIdentity :: FourId)
        , QC.testProperty "Four right" (propMonoidRightIdentity :: FourId)
        , QC.testProperty "BoolConj left" (propMonoidLeftIdentity :: BoolConjId)
        , QC.testProperty "BoolConj right" (propMonoidRightIdentity :: BoolConjId)
        , QC.testProperty "BoolDisj left" (propMonoidLeftIdentity :: BoolDisjId)
        , QC.testProperty "BoolDisj right" (propMonoidRightIdentity :: BoolDisjId)
        ]
