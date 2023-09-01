module IPV6 where

import Data.Word
import Text.Trifecta
import Numeric (readHex)
import Control.Applicative

data IPAddress6 = IPAddress6 Word64 Word64
    deriving (Eq, Ord, Show)

data AddressType = DoubleColon [Word16] | NoDoubleColon [Word16]
    deriving (Eq, Ord, Show)

type Placeholder = Bool

data IP6Segment = IP6Segment Word16 | Placeholder
    deriving (Eq, Ord)

instance Show IP6Segment where
    -- show (IP6Segment n) = showHex n ""
    show (IP6Segment n) = show n
    show Placeholder = "ph"

showIP6Hex :: IP6Segment -> Integer
showIP6Hex (IP6Segment n)  = toInteger n
showIP6Hex Placeholder = -1

hexStringToWord16 :: String -> Maybe Word16
hexStringToWord16 hexStr =
    case readHex hexStr of
        [(x, "")] -> Just x
        _ -> Nothing

parseSegment :: Parser IP6Segment
parseSegment = do
    a <- many $ notChar ':'
    let f :: String -> Parser IP6Segment
        f s | length s > 4 = unexpected $ "too many digits: " ++ show (length s)
        f s | null s = return Placeholder
        f s = case hexStringToWord16 s of
            Just x -> return $ IP6Segment x
            Nothing -> unexpected "bad hex value"
    f a

parsePlaceHolder :: Parser IP6Segment
parsePlaceHolder = pure Placeholder

parseSegments :: Parser [IP6Segment]
parseSegments = parseSegment `sepBy` char ':'

-- parseSegments :: Parser [IP6Segment]
-- parseSegments = try parseColonSegments2 <|> parseSegments1

parseColonSegments2 :: Parser [IP6Segment]
parseColonSegments2 = do
    _ <- char ':'
    parseSegments
