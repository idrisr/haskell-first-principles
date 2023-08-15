module ListTest (listTests) where

import List

import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Checkers
import Test.Tasty.HUnit

listTests :: TestTree
listTests = testGroup "List" [lawTests, unitTests]

lawTests :: TestTree
lawTests =
    let a :: Integer
        a = 10
        propsM :: [Test]
        propsM = unbatch $ monoid (Cons a Nil)
        xs :: List (Char, String, Integer)
        xs = Cons ('a', "a", 10) Nil
        propsA :: [Test]
        propsA = unbatch $ applicative xs
     in testGroup
            "List"
            [ QC.testProperties "Monoid" propsM
            , QC.testProperties "Applicative" propsA
            ]

unitTests :: TestTree
unitTests =
    let
        f :: List (Int -> Int)
        f = Cons (+ 1) (Cons (* 2) Nil)
        v = Cons 1 (Cons 2 Nil)
        a = Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
     in
        testGroup "List unit test" [testCase "" $ f <*> v @?= a]

-- Prelude> fmap (\x -> [x, 9]) [1, 2, 3]
-- [[1,9],[2,9],[3,9]]
-- Prelude> toMyList = foldr Cons Nil
-- Prelude> xs = toMyList [1, 2, 3]
-- Prelude> c = Cons
-- Prelude> f x = x `c` (9 `c` Nil)
-- Prelude> flatMap f xs
-- Cons 1 (Cons 9 (Cons 2
-- (Cons 9 (Cons 3 (Cons 9 Nil)))))
