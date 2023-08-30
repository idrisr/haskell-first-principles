module Main (main) where

import Data.IniTest
import EarlyTransTest
import FractionTests
import FreeJazzTest
import IPV4Test
import LogFileTest
import PhoneTest
import PositiveIntTest
import SemVerTest
import Test.Tasty
import TokenTest

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
        , ipv4tests
        ]

main :: IO ()
main = defaultMain tests
