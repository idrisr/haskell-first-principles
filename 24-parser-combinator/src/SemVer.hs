module SemVer where

import Text.Trifecta
import Text.Read

data NumberOrString = NOSS String | NOSI Integer
    deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
    deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer = do
    (mj, mn, p) <- parseMMP
    r <- option [] parseRelease
    m <- option [] parseMetadata
    -- return $ SemVer mj mn p r m
    return $ SemVer mj mn p r m

parseMMP :: Parser (Major, Minor, Patch)
parseMMP = do
{- hlint ignore "Use <$>" -}
    a <- decimal
    _ <- char '.'
    b <- decimal
    _ <- char '.'
    c <- decimal
    return $ (,,) a b c

parseRelease :: Parser Release
parseRelease =  do
    _ <- char '-'
    many parseNumberOrString

parseMetadata :: Parser Release
parseMetadata =  do
    _ <- char '+'
    many parseNumberOrString

parseNumberOrString :: Parser NumberOrString
parseNumberOrString =  do
    a <- some alphaNum
    skipPeriod
    case readMaybe a :: Maybe Integer of
        Nothing -> return $ NOSS a
        Just x -> return $ NOSI x

skipPeriod :: Parser ()
skipPeriod = skipMany $ oneOf "."
