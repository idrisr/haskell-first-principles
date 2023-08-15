module ZipListTest where

import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.QuickCheck.Checkers
import ZipList
import Prelude hiding (exp)
import Data.Monoid

zipListTests :: TestTree
zipListTests = testGroup "ZipList" [unitTests, functorTests, monoidTests, applicativeTests]

applicativeTests :: TestTree
applicativeTests =
    let
        xs :: ZipList' (Int, Char, String)
        xs = ZipList' [(10, 'a', "1")]
        props :: [Test]
        props = unbatch $ applicative xs
     in
        testGroup
            "Applicative"
            [ QC.testProperties "Applicative" props
            ]

functorTests :: TestTree
functorTests =
    let
        xs :: ZipList' (Int, Char, String)
        xs = ZipList' [(10, 'a', "1")]
        props :: [Test]
        props = unbatch $ functor xs
     in
        testGroup
            "Functor"
            [ QC.testProperties "Functor" props
            ]

monoidTests :: TestTree
monoidTests =
    let
        xs :: ZipList' (Sum Int)
        xs = ZipList' [10, 1, 2, 3]
        props :: [Test]
        props = unbatch $ monoid xs
     in
        testGroup
            "Monoid"
            [ QC.testProperties "Applicative" props
            ]

unitTests :: TestTree
unitTests =
    testGroup
        "unit tests"
        [ let
            a = ZipList' [(+ 9), (* 2), (+ 8)]
            b = ZipList' [1 .. 3]
            exp :: ZipList' Integer
            exp = ZipList' [10, 4, 11]
            got = a <*> b
           in
            testCase "" (got @?= exp)
        , let
            a = ZipList' [(+ 9), (* 2), (+ 8)]
            b = pure 1
            exp :: ZipList' Integer
            exp = ZipList' [10, 2, 9]
            got = a <*> b
           in
            testCase "" (got @?= exp)
        , let
            a = ZipList' [1, 2]
            exp :: ZipList' Integer
            exp = ZipList' [1, 2]
            {- hlint ignore "Use <$>" -}
            got = pure id <*> a
           in
            testCase "" (got @?= exp)
        ]
