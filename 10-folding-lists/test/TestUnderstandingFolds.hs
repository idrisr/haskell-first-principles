module TestUnderstandingFolds where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import FoldExamples

foldTests :: TestTree
foldTests = testGroup "folds" [testFoldr, testFoldl]

property1 :: (Num a, Eq a) => [a] -> Bool
{- hlint ignore "Use product" -}
property1 xs = foldr (*) 1 xs == foldl (flip (*)) 1 xs

propLFold :: [Int] -> Int -> Bool
propLFold xs b = foldl (+) b xs == myFoldl (+) b xs

propRFold :: Eq b => (a -> b -> b) -> [a] -> b -> Bool
propRFold f xs b = foldr f b xs == myFoldr f b xs

testFoldr :: TestTree
testFoldr = testGroup "foldr"
    [
    QC.testProperty "" $ \a -> property1 (a::[Int]),
    QC.testProperty "" $ \a b -> propRFold (++) (a::[String]) (b::String)
    ]

testFoldl :: TestTree
testFoldl = testGroup "foldl"
    [
    QC.testProperty "" $ \a b -> propLFold a b
    ]
