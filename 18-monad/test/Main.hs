module Main (main) where

import Test.Tasty
import BadMonadTest
import BahEitherTest
import NopeTest
import IdentityTest

main :: IO ()
main =
    defaultMain $
        testGroup
            "main"
            [ badMonadTests
            , bahEitherTests
            , identityTests
            , nopeTests
            ]
