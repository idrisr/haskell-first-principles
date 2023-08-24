{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)

-- import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M

-- import Data.Text (Text)
-- import qualified Data.Text.IO as TIO
import Text.RawString.QQ
import Text.Trifecta

headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String
    deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
    char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

-- letter :: CharParsing m => m Char
-- some ::   Alternative f => f a => f [a]
-- some letter :: CharParsing f => f [Char]

-- h :: CharParsing m => m Header
-- h = Header <$> some letter

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    _ <- char '='
    val <- some (noneOf "\n")
    _ <- skipEOL
    return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

commentEx :: ByteString
commentEx = "; last modified 1 April 2001 by John Doe"

commentEx1 :: ByteString
commentEx1 = "; blah\n; woot\n \n;hah"

skipComments :: Parser ()
skipComments =
    skipMany
        ( do
            _ <- char ';' <|> char '#'
            skipMany (noneOf "\n")
            skipEOL
        )

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx1 :: ByteString
sectionEx1 =
    [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx2 :: ByteString
sectionEx2 =
    [r|
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]

data Section = Section Header Assignments
    deriving (Eq, Show)

newtype Config = Config (Map Header Assignments)
    deriving (Eq, Show)

skipWhiteSpace :: Parser ()
skipWhiteSpace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
    skipWhiteSpace
    skipComments
    h <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
{- hlint ignore "Eta reduce" -}
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections = foldr rollup M.empty sections
    return (Config mapOfSections)
