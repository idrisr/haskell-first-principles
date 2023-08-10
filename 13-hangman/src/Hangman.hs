module Hangman (zipper, freshPuzzle, fillInCharacter) where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

minWordLength = 5::Int
maxWordLength = 9::Int
type WordList = [String]
data Puzzle = Puzzle String [Maybe Char] [Char]
    deriving (Eq)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs ns []
    where ns = flip replicate Nothing $ length xs

instance Show Puzzle where
    show (Puzzle _ d g) = u ++ " Guessed so far: " ++ g
        where a = fmap renderPuzzleChar d
              u = intersperse ' ' a

allWords :: IO WordList
allWords = readFile "data/dict.txt" >>= \dict -> pure $ lines dict

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ gs) g = g `elem` gs

-- raj [Nothing Nothing Nothing] []
-- guess a
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle s xs ys) c = Puzzle s xs' ys'
    where xs' = zipWith (zipper c) ys xs
          ys' = c:ys

zipper:: Eq a => a -> a -> Maybe a -> Maybe a
zipper a b c | a==b = Just b
             | otherwise = c

gameWords :: IO WordList
gameWords = filter gameLength <$> allWords
    where gameLength w =
            let l = length (w::String)
            in  l >= minWordLength
            &&  l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = randomRIO (0, length wl - 1) >>=
    \randomIndex -> pure $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

main :: IO ()
main = print "theres a tramp sitting on my doorstep"
