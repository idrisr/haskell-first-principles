module FuncsTest where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Funcs
import Instances
import Rearrange

funcTests :: TestTree
funcTests = testGroup "Functor Laws" [idTests, compTests]

propId :: (Functor f, Eq (f a)) => f a -> Bool
{- hlint ignore "Functor law" -}
propId x = fmap id x == x

propComp :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
propComp f g x = (fmap f . fmap g) x == fmap (f . g) x

idTests :: TestTree
idTests =
    testGroup
        "id laws"
        [ QC.testProperty "WhoCares" (propId :: (WhoCares Int -> Bool))
        , QC.testProperty "Pair" (propId :: (Pair Integer -> Bool))
        , QC.testProperty "Two" (propId :: (Two Integer String -> Bool))
        , QC.testProperty "Three" (propId :: (Three Integer String (Maybe Int) -> Bool))
        , QC.testProperty "Three'" (propId :: (Three' String (Maybe Int) -> Bool))
        , QC.testProperty "Four" (propId :: (Four Char (Maybe [Int]) String (Maybe Int) -> Bool))
        , QC.testProperty "Four'" (propId :: (Four' Char [Int] -> Bool))
        , QC.testProperty "Constant'" (propId :: (Constant Char [Int] -> Bool))
        , QC.testProperty "More'" (propId :: (More Int Int -> Bool))
        , QC.testProperty "Quant'" (propId :: Quant Int [Integer] -> Bool)
        , QC.testProperty "K'" (propId :: K Int [Integer] -> Bool)
        , QC.testProperty "EvilGoateeConst" (propId :: EvilGoateeConst Int [Integer] -> Bool)
        , QC.testProperty "LiftItOut" (propId :: LiftItOut Maybe [Integer] -> Bool)
        , QC.testProperty "Parappa" (propId :: Parappa Maybe [] [Integer] -> Bool)
        , QC.testProperty "IgnoreOne" (propId :: IgnoreOne Maybe (Either String) Int Char -> Bool)
        , QC.testProperty "Notorious" (propId :: Notorious Maybe String Double Int -> Bool)
        , QC.testProperty "List" (propId :: List Int -> Bool)
        , QC.testProperty "GoatLord" (propId :: GoatLord [Integer]-> Bool)
        ]

compTests :: TestTree
compTests =
    let
        f x = x + 2 - 3 * 5
        g x = x - 4 * 8 `div` 17
     in
        testGroup
            "composition laws"
            [ QC.testProperty "WhoCares" $ \a -> propComp f g (a :: WhoCares Integer)
            , QC.testProperty "Pair" $ \a -> propComp f g (a :: Pair Integer)
            , QC.testProperty "Two" $ \a -> propComp f g (a :: Two String Integer)
            , QC.testProperty "Three" $ \a -> propComp f g (a :: Three String Integer Integer)
            , QC.testProperty "Three'" $ \a -> propComp f g (a :: Three' String Integer)
            , QC.testProperty "Four" $ \a -> propComp f g (a :: Four Char [Integer] String Integer)
            , QC.testProperty "Four'" $ \a -> propComp f g (a :: Four' [Integer] Integer)
            , QC.testProperty "Constant" $ \a -> propComp f g (a :: Constant [Integer] Integer)
            , QC.testProperty "More" $ \a -> propComp f g (a :: More [Integer] Integer)
            , QC.testProperty "Quant" $ \a -> propComp f g (a :: Quant [Integer] Integer)
            , QC.testProperty "K" $ \a -> propComp f g (a :: K [Integer] Integer)
            , QC.testProperty "LiftItOut" $ \a -> propComp f g (a :: LiftItOut (Either String) Integer)
            , QC.testProperty "Parappa" $ \a -> propComp f g (a :: Parappa (Either String) Maybe Integer)
            , QC.testProperty "IgnoreOne" $ \a -> propComp f g (a :: IgnoreOne (Either String) Maybe Char Integer)
            , QC.testProperty "Notorious" $ \a -> propComp f g (a :: Notorious Maybe Char Float Integer)
            , QC.testProperty "List" $ \a -> propComp f g (a :: List Integer)
            , QC.testProperty "GoatLord" $ \a -> propComp f g (a :: GoatLord Integer)
            ]
