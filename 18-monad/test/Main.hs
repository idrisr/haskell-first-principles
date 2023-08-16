module Main (main) where

import Test.Tasty
import BadMonadTest
import BahEitherTest
import NopeTest
import IdentityTest
import ListTest
import FuncsTest

main :: IO ()
main =
    defaultMain $
        testGroup
            "main"
            [ badMonadTests
            , bahEitherTests
            , identityTests
            , nopeTests
            , listTests
            , funcsTests
            ]
