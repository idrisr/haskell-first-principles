module ElemTests (elemTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified LibFuncs as LF
import Data.Monoid

elemTests :: TestTree
elemTests = testGroup "Elem" [propTests, unitTests]

elemProp :: (Foldable t, Eq a) => a -> t a -> Bool
elemProp x xs = LF.elem x xs == elem x xs

type IntProp = Int -> [Int] -> Bool
type TupProp =
    (String, [Int]) ->
    [(String, [Int])] ->
    Bool
type BoolProp = Bool -> [Bool] -> Bool

propTests :: TestTree
propTests =
    testGroup
        "prop"
        [ testProperty "" (elemProp :: IntProp)
        , testProperty "" (elemProp :: TupProp)
        , testProperty "" (elemProp :: BoolProp)
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit"
        [ let
            xs :: Sum Int
            xs = 100
            got = elem 1 xs
            wot = False
           in
            testCase "Sum" $ got @?= wot
        , let
            xs :: Sum Int
            xs = 100
            got = elem 100 xs
            wot = True
           in
            testCase "Sum" $ got @?= wot
        , let
            xs :: (String, Int)
            xs = ("YO", 100)
            got = elem 100 xs
            wot = True
           in
            testCase "()" $ got @?= wot
        , let
            xs :: (String, Int)
            xs = ("YO", 100)
            got = elem 1 xs
            wot = False
           in
            testCase "()" $ got @?= wot
        ]
