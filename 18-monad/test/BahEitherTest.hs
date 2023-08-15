module BahEitherTest (bahEitherTests) where

import Data.Monoid
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck
import BahEither

bahEitherTests :: TestTree
bahEitherTests =
    testGroup
        "BahEither"
        [ functorTests
        , applicativeTests
        , monadTests
        ]

xs ::
    BahEither
        (String, Sum Int, Product Int)
        (Sum Int, Product Int, String)
xs = PLeft (10, 11, "we")

functorTests :: TestTree
functorTests =
    let props = unbatch $ functor xs
     in testGroup "Functor" [ testProperties "" props ]

applicativeTests :: TestTree
applicativeTests =
    let props = unbatch $ applicative xs
     in testGroup "Applicative" [ testProperties "" props ]

monadTests :: TestTree
monadTests =
    let props = unbatch $ monad xs
     in testGroup "Monad" [testProperties "" props]
