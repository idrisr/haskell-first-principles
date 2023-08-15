module TwoTest (twoTests) where

import Two
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Checkers
import Data.Monoid

twoTests :: TestTree
twoTests = testGroup "Two" [functorTests, applicativeTests]

xs :: Two (String, Sum Int, Product Int) (String, Sum Int, Product Int)
xs = Two a a
    where a = ("YO", 10, 69)

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
