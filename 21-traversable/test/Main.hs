module Main (main) where

import Test.Tasty
import BabyStepTest
import EitherTests
import IdentityTests
import ConstantTests
import OptionalTests
import ListTests
import ThreeTests
import PairTests
import BigTests
import BiggerTests
import TreeTests
import SkiFreeTests

tests :: TestTree
tests =
    testGroup
        "Main"
        [ babyStepTests
        , eitherTests
        , identityTests
        , constantTests
        , optionalTests
        , listTests
        , threeTests
        , pairTests
        , bigTests
        , biggerTests
        , treeTests
        , skiFreeTests
        ]

main :: IO ()
main = defaultMain tests
