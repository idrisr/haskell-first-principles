module TestFoldPractice (pracTests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import FoldPractice

pracTests :: TestTree
pracTests = testGroup "fold practice" [anyTests, orTests, elemTests, revTests]

propOr :: ([Bool] -> Bool) -> [Bool] -> Bool
propOr f xs = f xs == or xs

propAny :: ((a -> Bool) -> [a] -> Bool) -> (a -> Bool) -> [a] -> Bool
propAny g f xs = g f xs == any f xs

propElem :: Eq a => (a -> [a] -> Bool) -> a -> [a] -> Bool
propElem f y xs = f y xs == elem y xs

propRev :: Eq a => ([a] -> [a]) -> [a] -> Bool
propRev f xs = f xs == reverse xs

orTests :: TestTree
orTests = testGroup "myOr" [
    QC.testProperty "or1" $ propOr myOr1
  , QC.testProperty "or2" $ propOr myOr2
  , QC.testProperty "or3" $ propOr myOr3
  , QC.testProperty "or4" $ propOr myOr4
    ]

anyTests :: TestTree
anyTests = testGroup "myAny" [
    QC.testProperty "any1" $ \xs -> propAny myAny1 even (xs::[Integer])
  , QC.testProperty "any2" $ \xs -> propAny myAny2 even (xs::[Integer])
  , QC.testProperty "any3" $ \xs -> propAny myAny3 even (xs::[Integer])
  , QC.testProperty "any4" $ \xs -> propAny myAny4 even (xs::[Integer])
    ]

elemTests :: TestTree
elemTests = testGroup "myElem" [
    QC.testProperty "elem1" $ \y xs -> propElem myElem1 y (xs::[Integer])
  , QC.testProperty "elem2" $ \y xs -> propElem myElem2 y (xs::[Integer])
  , QC.testProperty "elem3" $ \y xs -> propElem myElem3 y (xs::[Integer])
  , QC.testProperty "elem4" $ \y xs -> propElem myElem4 y (xs::[Integer])
    ]

revTests :: TestTree
revTests = testGroup "myRev" [
    QC.testProperty "rev" $ \xs -> propRev myReverse1 (xs::String)
  , QC.testProperty "rev" $ \xs -> propRev myReverse2 (xs::String)
  , QC.testProperty "rev" $ \xs -> propRev myReverse3 (xs::String)
    ]
