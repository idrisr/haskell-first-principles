module BabyStepTest (babyStepTests) where

import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

babyStepTests :: TestTree
babyStepTests =
    testGroup
        "BabySteps"
        [ unitTests1
        , unitTests2
        , unitTests3
        ]

unitTests1 :: TestTree
unitTests1 =
    testGroup
        -- remove inner type constructor
        "one way"
        [ let wot :: Integer
              wot = 6
              got = sum [1, 2, 3]
           in testCase "a" $ wot @?= got
        , let xs :: [Maybe Integer]
              xs = [Just 1, Just 2, Just 3]
              got :: [Integer]
              got = fmap sum xs
              wot = [1, 2, 3]
           in testCase "b" $ wot @?= got
        , let
            got = (fmap . fmap) sum Just [1, 2, 3]
            wot :: Maybe Integer
            wot = Just 6
           in
            testCase "c what in the fuck" $ wot @?= got
        , let
            wot :: [Integer]
            wot = [1, 2, 1]
            xs = [Just 1, Just 2, Nothing]
            f :: (Num a, Functor f, Foldable t) => f (t a) -> f a
            f = fmap product
            got = f xs
           in
            testCase "d" $ wot @?= got
        ]

unitTests2 :: TestTree
unitTests2 =
    testGroup
        "from t f to f t"
        [ let
            got :: [Maybe Integer]
            got = fmap Just [1, 2, 3]
            wot = [Just 1, Just 2, Just 3]
           in
            testCase "e" $ wot @?= got
        , {- hlint ignore "Use traverse" -}
          let got = sequenceA $ fmap Just [1, 2, 3]
              wot :: Maybe [Integer]
              wot = Just [1, 2, 3]
           in testCase "f" $ wot @?= got
        , let
            xs :: [Maybe Integer]
            xs = [Just 1, Just 2, Just 3]
            got = sequenceA xs
            wot = Just [1, 2, 3]
           in
            testCase "g" $ wot @?= got
        , let
            xs :: [Maybe Integer]
            xs = [Just 1, Just 2, Nothing]
            got = sequenceA xs
            wot = Nothing
           in
            testCase "h" $ wot @?= got
        , let
            xs :: [Maybe Integer]
            xs = [Just 1, Just 2, Nothing]
            got = sum <$> sequenceA xs
            wot = Nothing
           in
            testCase "j" $ wot @?= got
        , let
            xs :: [Maybe Integer]
            xs = [Just 1, Just 2]
            got = sum <$> sequenceA xs
            wot = Just 3
           in
            testCase "j2" $ wot @?= got
        , let
            xs :: [Maybe Integer]
            xs = [Just 1, Just 2, Nothing]
            got = sum <$> sequenceA xs
            wot = Nothing
           in
            testCase "k" $ wot @?= got
        ]

unitTests3 :: TestTree
unitTests3 =
    testGroup
        "stuff"
        [ let
            xs :: [Maybe Integer]
            xs = [Just 1, Just 2, Just 3]
            got = catMaybes xs
            wot = [1, 2, 3]
           in
            testCase "e" $ wot @?= got
        ]
