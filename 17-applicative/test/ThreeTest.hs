module ThreeTest (threeTests) where

import Three
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Checkers
import Data.Monoid

threeTests :: TestTree
threeTests = testGroup "Three" [functorTests, applicativeTests]

xs ::
    Three
        (String, Sum Int, Product Int)
        (String, Sum Int, Product Int)
        (String, Sum Int, Product Int)
xs = Three a a a
  where
    a = ("YO", 10, 69)

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
