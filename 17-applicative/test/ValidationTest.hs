module ValidationTest (validationTests) where

import Validation
import Test.QuickCheck.Classes
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.QuickCheck.Checkers
import Prelude hiding (exp)

validationTests :: TestTree
validationTests = testGroup "Validation" [unitTests, functorTests, applicativeTests]

data Errors =
    DividedByZero
    | StackOverflow
    | MooglesChewedWires
    deriving (Eq, Show)

unitTests :: TestTree
unitTests =
    testGroup
        "unit tests"
        [ let
            got :: Validation String Int
            got = Success (+1) <*> Success 1
            exp = Success 2
           in
            testCase "" $ got @?= exp
        , let
            got :: Validation [Errors] Int
            got = Success (+1) <*> Failure [StackOverflow]
            exp = Failure [StackOverflow]
           in
            testCase "" $ got @?= exp
        , let
            got :: Validation [Errors] Int
            got = Failure [MooglesChewedWires] <*> Failure [StackOverflow]
            exp = Failure [MooglesChewedWires , StackOverflow]
           in
            testCase "" $ got @?= exp
        ]

functorTests :: TestTree
functorTests =
    let
        xs :: Validation String (Int, Char, String)
        xs = Success (10, 'a', "1")
        props :: [Test]
        props = unbatch $ functor xs
     in
        testGroup
            "Functor"
            [ QC.testProperties "Functor" props
            ]

applicativeTests :: TestTree
applicativeTests =
    let
        xs :: Validation String (Int, Char, String)
        xs = Success (10, 'a', "1")
        props :: [Test]
        props = unbatch $ applicative xs
     in
        testGroup
            "Applicative"
            [ QC.testProperties "Applicative" props
            ]
