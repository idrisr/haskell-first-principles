module PairTest where

import Pair
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Checkers

pairTests :: TestTree
pairTests = testGroup "Pair" [functorTests, applicativeTests]

xs :: Pair (String, Int, Char)
xs = Pair a a
    where a = ("YO", 10, 'a')

functorTests :: TestTree
functorTests =
    let
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
        props :: [Test]
        props = unbatch $ applicative xs
     in
        testGroup
            "Applicative"
            [ QC.testProperties "Applicative" props
            ]
