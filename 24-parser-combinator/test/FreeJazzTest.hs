module FreeJazzTest where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta

freeJazzTests :: TestTree
freeJazzTests = testGroup "Free Jazz" [unitTests]

instance Eq a => Eq (Result a) where
    (Success a) == (Success a1) = a == a1
    (Failure e) == (Failure e1) = show e == show e1
    _ == _ = False

unitTests :: TestTree
unitTests = testGroup "" [
    let
        gimmeA :: Parser Char
        gimmeA = char 'a'
        got :: Result Char
        got = parseString gimmeA mempty "a"
        wot :: Result Char
        wot = Success 'a' in
        testCase "" $ got @?= wot
    ]
