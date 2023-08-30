module Main (main) where

import Data.IniTest
import EarlyTransTest
import FractionTests
import FreeJazzTest
import PhoneTest
import PositiveIntTest
import SemVerTest
import Test.Tasty
import TokenTest
import LogFileTest

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
        , posIntTests
        , phoneTests
        , logFileTests
        ]

main :: IO ()
main = defaultMain tests
