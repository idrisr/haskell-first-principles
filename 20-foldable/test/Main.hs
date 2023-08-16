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
        ""
        [ funcTests
        , minTests
        , maxTests
        , elemTests
        , nullTests
        , lengthTests
        , toListTests
        , toListTests
        , constantTests
        , twoTests
        , threeTests
        , three1Tests
        , four1Tests
        ]

main :: IO ()
main = defaultMain tests
