module TestSemigroup where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC hiding (Failure, Success)
import Properties
import Semigroup
import Data.Monoid

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

type TwoAssoc =
    Two String [Int] ->
    Two String [Int] ->
    Two String [Int] ->
    Bool

type ThreeAssoc =
    Three String [Int] (Sum Int) ->
    Three String [Int] (Sum Int) ->
    Three String [Int] (Sum Int) ->
    Bool

type FourAssoc =
    Four String [Int] (Sum Int) (Product Int) ->
    Four String [Int] (Sum Int) (Product Int) ->
    Four String [Int] (Sum Int) (Product Int) ->
    Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type Ort = Or (Sum Int) (Product [Int])
type OrAssoc = Ort -> Ort -> Ort -> Bool

type Combinet = Combine String (Maybe String)
type CombineAssoc = Combinet -> Combinet -> Combinet -> Bool
type Vt = Validation String Int
type ValidationAssoc = Vt -> Vt -> Vt -> Bool

semigroupTests :: TestTree
semigroupTests =
    testGroup
        "semigroup"
        [ assocTest
        , boolConjTests
        , boolDisjTests
        , orTests
        , combineTests
        , valTests
        ]

assocTest :: TestTree
assocTest =
    testGroup
        "associativity"
        [ QC.testProperty "trivial" (propSemiAssoc :: TrivAssoc)
        , QC.testProperty "identity" (propSemiAssoc :: IdentityAssoc)
        , QC.testProperty "two" (propSemiAssoc :: TwoAssoc)
        , QC.testProperty "three" (propSemiAssoc :: ThreeAssoc)
        , QC.testProperty "four" (propSemiAssoc :: FourAssoc)
        , QC.testProperty "BoolConj" (propSemiAssoc :: BoolConjAssoc)
        , QC.testProperty "BoolDisj" (propSemiAssoc :: BoolDisjAssoc)
        , QC.testProperty "Or" (propSemiAssoc :: OrAssoc)
        -- , QC.testProperty "Combine" (propSemiAssoc :: CombineAssoc)
        , QC.testProperty "Validation" (propSemiAssoc :: ValidationAssoc)
        ]

boolConjTests :: TestTree
boolConjTests =
    testGroup
        "BoolConj"
        [ let got = BoolConj True <> BoolConj True
              sb = BoolConj True
           in testCase "and 1" $ got `compare` sb @?= EQ
        , let got = BoolConj True <> BoolConj False
              sb = BoolConj False
           in testCase "and 2" $ got `compare` sb @?= EQ
        ]

boolDisjTests :: TestTree
boolDisjTests =
    testGroup
        "BoolDisj"
        [ let got = BoolDisj True <> BoolDisj True
              sb = BoolDisj True
           in testCase "and 1" $ assertEqual "" got sb
        , let got = BoolDisj True <> BoolDisj False
              sb = BoolDisj True
           in testCase "and 2" $ assertEqual "" got sb
        ]

orTests :: TestTree
orTests =
    testGroup
        "Or"
        [ let got = Fst 1 <> Snd 2
              sb = Snd 2
              sb :: Or Int Int
           in testCase "1" $ got @=? sb
        , let got = Fst 1 <> Fst 2
              sb :: Or Int Int
              sb = Fst 2
              in testCase "2" $ got @=? sb
        , let got = Snd 1 <> Fst 2
              sb = Snd 1
              sb :: Or Int Int
              in testCase "3" $ got @=? sb
        , let got = Snd 1 <> Snd 2
              sb :: Or Int Int
              sb = Snd 1
           in testCase "4" $ got @=? sb
        ]

combineTests :: TestTree
combineTests =
    testGroup
        "Combine"
        $ let f = Combine $ \n -> Sum (n + 1)
              g = Combine $ \n -> Sum (n - 1)
           in [ let
                    got = unCombine (f <> g) 0
                    sb :: Sum Int
                    sb = Sum{getSum = 0}
                 in
                    testCase "1" $ got @=? sb
              , let
                    got = unCombine (f <> g) 1
                    sb :: Sum Int
                    sb = Sum{getSum = 2}
                 in
                    testCase "2" $ got @=? sb
              , let
                    got = unCombine (f <> f) 1
                    sb :: Sum Int
                    sb = Sum{getSum = 4}
                 in
                    testCase "3" $ got @=? sb
              , let
                    got = unCombine (f <> f) 1
                    sb :: Sum Int
                    sb = Sum{getSum = 4}
                 in
                    testCase "3" $ got @=? sb
              ]

valTests :: TestTree
valTests =
    testGroup
        "Validation"
        $ let failure :: String -> Validation String Int
              failure = Failure
              success :: Int -> Validation String Int
              success = Success
           in [ let got = success 1 <> failure "blah"
                    sb = Success 1
                 in testCase "1" $ got @=? sb
              , let got = failure "woot" <> failure "blah"
                    sb = Failure "wootblah"
                 in testCase "2" $ got @=? sb
              , let got = success 1 <> success 2
                    sb = Success 1
                 in testCase "3" $ got @=? sb
              , let got = failure "woot" <> success 2
                    sb = Success 2
                 in testCase "4" $ got @=? sb
              ]
