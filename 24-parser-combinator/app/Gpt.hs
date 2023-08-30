module Main where

import Text.Trifecta
import Control.Applicative
import Text.Parser.LookAhead

lineParser :: Parser String
lineParser = do
    line <- manyTill anyChar (try commentStart <|> lookAhead newline)
    _ <- optional (char '\n')
    return line

commentStart :: Parser ()
commentStart = string "--" *> skipMany (noneOf "\n") *> optional (char '\n')

parseInput :: String -> Result [String]
parseInput = parseString (many lineParser) mempty

sampleInput :: String
sampleInput =
    "08:00 something --comment\n\
    \Line 1\n\
    \-- Another comment\n\
    \Line 2\n"

main :: IO ()
main = case parseInput sampleInput of
    Success lines -> print lines
    Failure err   -> print err
