module Main where

data Day = M | T | W | Th | F | Sa | Su
    deriving (Show, Enum)

data Letter = A | B | C | D | E | G
    deriving (Show, Enum)

class Booly a where
    truthy :: a -> Bool

instance Booly Day where
    truthy M = True
    truthy T = True
    truthy _ = False

instance Booly Letter where
    truthy A = True
    truthy E = True
    truthy _ = False

goodDay :: Day -> Bool
goodDay M = False
goodDay _ = True

x :: [Day]
x = y Th

y :: (Booly a, Enum a) => a -> [a]
y = take 5 . filter truthy . enumFrom

main :: IO ()
main = print "YO"
