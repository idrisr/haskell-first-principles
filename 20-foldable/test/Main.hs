module Main (main) where

import Test.Tasty
import LibFuncsTest
import MinimumTests
import MaximumTests
import ElemTests
import NullTests
import LengthTests
import ToListTests
import ConstantTests
import TwoTests
import ThreeTests
import Three1Tests
import Four1Tests

tests :: TestTree
tests =
    testGroup
        "main"
        [
        constantTests
        , elemTests
        , four1Tests
        , funcTests
        , lengthTests
        , maxTests
        , minTests
        , nullTests
        , three1Tests
        , threeTests
        , toListTests
        , twoTests
        ]

main :: IO ()
main = defaultMain tests
