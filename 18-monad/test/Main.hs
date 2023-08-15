module Main (main) where

import Test.Tasty
import BadMonadTest
import BahEitherTest
import NopeTest

main :: IO ()
main =
    defaultMain $
        testGroup
            "main"
            [ badMonadTests
            , nopeTests
            , bahEitherTests
            ]
