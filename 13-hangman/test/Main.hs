module Main where

import Test.Hspec
import Hangman

spec :: Spec
spec =
    describe "Puzzle" $ do
        it "1 fill in one char" $ do
            show np `shouldBe` "_ d _ _ _ Guessed so far: d"
            where p = freshPuzzle "idris"
                  np = fillInCharacter p 'd'

spec1 :: SpecWith ()
spec1 =
    describe "Puzzle" $ do
        it "1 fill in one char" $ do
            show np `shouldBe` "_ d _ _ _ Guessed so far: d"
            where p = freshPuzzle "idris"
                  np = fillInCharacter p 'd'

spec2 :: SpecWith ()
spec2 =
    describe "zipper" $ do
        it "not equal" $ do
            zipper 'c' 'd' (Just 'e') `shouldNotBe` Just 'd'
        it "equals" $ do
            zipper 'c' 'd' (Just 'e') `shouldBe` Just 'e'

fillInCharSpec :: SpecWith ()
fillInCharSpec =
    describe "Puzzle" $ do
        it "fresh" $ do
            show (freshPuzzle "Idris") `shouldBe` "_ _ _ _ _ Guessed so far: "

        let p = freshPuzzle "idris"
            np = fillInCharacter p 'd' in
            it "fill in one char" $ do
                show np `shouldNotBe` "d _ _ _ _ Guessed so far: "

        let p = freshPuzzle "idris"
            np = fillInCharacter p 'd' in
            it "fill in one char" $ do
                show np `shouldBe` "_ d _ _ _ Guessed so far: d"

        let p = freshPuzzle "idris"
            np = fillInCharacter p 'd'
            npp = fillInCharacter np 'i' in
            it "fill in two chars" $ do
                show npp `shouldBe` "i d _ i _ Guessed so far: id"

main :: IO ()
main =
    hspec spec1
    >> hspec spec
    -- >> hspec spec2
    -- >> hspec fillInCharSpec
