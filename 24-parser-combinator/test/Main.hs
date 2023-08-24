module Main (main) where

import Data.IniTest
import EarlyTransTest
import FractionTests
import FreeJazzTest
import Test.Tasty
import TokenTest
import SemVerTest

tests :: TestTree
tests =
    testGroup
        "Main"
        [ testsEarly
        , freeJazzTests
        , fractionTests
        , iniTests
        , tokenTests
        , semVerTests
        ]

main :: IO ()
main = defaultMain tests
