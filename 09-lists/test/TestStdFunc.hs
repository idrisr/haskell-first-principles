module TestStdFunc where

import StdFunc

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

myAndProp :: [Bool] -> Bool
myAndProp xs = myAnd xs == and xs

myOrProp :: [Bool] -> Bool
myOrProp xs = myOr xs == or xs

myAnyProp :: (a -> Bool) -> [a] -> Bool
myAnyProp f xs = myAny f xs == any f xs

myElemProp :: Eq a => a -> [a] -> Bool
myElemProp x xs = myElem x xs == elem x xs

myReverseProp :: Eq a => [a] -> Bool
myReverseProp xs = myReverse xs == reverse xs

squishProp :: Eq a => [[a]] -> Bool
squishProp xs = squish xs == concat xs

squishMapProp :: Eq b => (a->[b])-> [a] -> Bool
squishMapProp f xs = squishMap f xs == concatMap f xs

myMaximumProp :: [Int] -> QC.Property
myMaximumProp xs = not (null xs) QC.==> myMaximum xs == maximum xs

myMinimumProp :: [Int] -> QC.Property
myMinimumProp xs = not (null xs) QC.==> myMinimum xs == maximum xs


stdFuncProps = testGroup "standard list functions"
    [
    QC.testProperty "myAnd"       $ \a -> myAndProp (a::[Bool]),
    QC.testProperty "myOr"        $ \a -> myOrProp (a::[Bool]),
    QC.testProperty "myAny"       $ \a -> myAnyProp even (a::[Int]),
    QC.testProperty "myElem"      $ \a b -> myElemProp (a::Int) b,
    QC.testProperty "myReverse"   $ \a -> myReverseProp (a::[Ordering]),
    QC.testProperty "squish"      $ \a -> squishProp (a::[[Integer]]),
    QC.testProperty "squishMap"   $ \a -> squishMapProp (replicate 4) (a::[Integer]),
    QC.testProperty "myMaximum"     myMaximumProp,
    QC.testProperty "myMinimum"     myMinimumProp
    ]
