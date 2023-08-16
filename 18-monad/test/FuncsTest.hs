module FuncsTest where

import Test.Tasty.HUnit
import Test.Tasty
import Funcs

funcsTests :: TestTree
funcsTests = testGroup "Funcs"  [jTest]

jTest :: TestTree
jTest = testGroup "join" [
    let a = [[1, 2], [], [3]]
        wot :: [Integer]
        wot = [1,2,3]
        got = j a in
        testCase "join 1" (wot @?= got)
  , let a = Just (Just 1)
        wot :: Maybe Int
        wot = Just 1
        got = j a in
        testCase "join 2" (wot @?= got)
  , let a = Just Nothing
        wot :: Maybe Int
        wot = Nothing
        got = j a in
        testCase "join 3" (wot @?= got)
  , let a = Nothing
        wot :: Maybe Int
        wot = Nothing
        got = j a in
        testCase "join 4" (wot @?= got)
        ]
