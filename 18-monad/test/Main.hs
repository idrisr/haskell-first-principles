module Main (main) where

import Test.Tasty
import BadMonadTest
import NopeTest

main :: IO ()
main =
    defaultMain $
        testGroup
            "main"
            [ badMonadTests
            , nopeTests
            ]
