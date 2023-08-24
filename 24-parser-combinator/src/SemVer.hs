module SemVer where

import Text.Trifecta

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
    a <- decimal
    _ <- char '.'
    b <- decimal
    _ <- char '.'
    c <- decimal
    return $ SemVer a b c [] []
