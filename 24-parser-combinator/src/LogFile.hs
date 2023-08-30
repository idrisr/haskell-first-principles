-- [># LANGUAGE QuasiQuotes #<]

module LogFile where

import Text.Trifecta
import Control.Applicative
-- import Text.RawString.QQ

type Start = String
type End = String
type Desc = String
type Comment = String
type Date = (Int, Int, Int)
type Time = (Int, Int)
type Name = String

-- literal thing in the log file
data Entry = Entry Time Name Desc
    deriving Eq

data Day = Day Date [Entry]
    deriving Eq

instance Show Day where
    show (Day d es) = concat [show d, "\n", show es]

instance Show Entry where
    show (Entry t n d) = concat [show t, show n, show d, "\n"]

-- computes time from log file
-- dependent on adjacent values
data Activity = Activity Time Name
    deriving (Eq, Show)

parseFile :: Parser [Day]
parseFile = many parseDay

parseDay :: Parser Day
parseDay = do
    whiteSpace
    d <- parseDate
    e <- many parseEntry
    whiteSpace
    return $ Day d e

parseTime :: Parser Time
parseTime = do
    h <- integer
    _ <- char ':'
    m <- integer
    return (fromInteger h, fromInteger m)

parseDate :: Parser Date
parseDate = do
    _ <- char '#'
    whiteSpace
    y <- integer
    _ <- char '-'
    m <- integer
    _ <- char '-'
    d <- integer
    _ <- option '_' newline
    return (fromInteger y, fromInteger m, fromInteger d)

parseActivity :: Parser Activity
parseActivity = undefined

parseEntry :: Parser Entry
parseEntry  = do
    a <- try parseEntryC <|> parseEntryNC
    whiteSpace
    return a

parseEntryC :: Parser Entry
parseEntryC = do
    t <- parseTime
    whiteSpace
    m <- manyTill (notChar '\n') (try (string " --" >> many (notChar '\n')))
    let (n, d) = break (==',') m
    return $ Entry t n d

parseEntryNC :: Parser Entry
parseEntryNC = do
    t <- parseTime
    whiteSpace
    n <- some $ noneOf "\n,"
    d <- option "" $ comma >> many (notChar '\n')
    return $ Entry t n d

activity :: Day -> [Activity]
activity = undefined
