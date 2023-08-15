module Main (main) where

import ZipListTest
import ListTest
import ValidationTest
import Test.Tasty
import PairTest
import TwoTest
import ThreeTest
import Three1Test
import FourTest
import Four1Test

main :: IO ()
main =
    defaultMain $
        testGroup
            "main"
            [ zipListTests
            , listTests
            , validationTests
            , pairTests
            , twoTests
            , threeTests
            , three1Tests
            , fourTests
            , four1Tests
            ]
