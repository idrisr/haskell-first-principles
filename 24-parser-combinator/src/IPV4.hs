module IPV4 where

import Control.Applicative
import Data.CharSet
import Data.Word
import Text.Trifecta
import Data.Bits

newtype IPAddress = IPAddress Word32
    deriving (Eq, Ord, Show)

parseIPV4 :: Parser IPAddress
parseIPV4 = do
    xs <- parseSegments
    if length xs == 4
        then pure $ IPAddress (ipv4ListToWord32 xs)
        else unexpected "invalid Segment length" >> pure (IPAddress 0)

parseSegments :: Parser [Int]
parseSegments = parseSegment `sepBy` char '.'

parseSegment :: Parser Int
parseSegment = try parse0 <|> parseS

parse0 :: Parser Int
parse0 = do
    a <- char '0'
    notFollowedBy $ notChar '.'
    let x :: Int
        x = read [a]
    pure x

parseS :: Parser Int
parseS = do
    a <- oneOfSet $ fromList "123456789"
    b <- many $ oneOfSet $ fromList "0123456789"
    let x :: Int
        x = read $ a : b
    if x > 255
        then unexpected "invalid Segment" >> pure 0
        else pure x

ipv4ListToWord32 :: [Int] -> Word32
ipv4ListToWord32 [a, b, c, d] =
    fromIntegral a `shiftL` 24
        .|. fromIntegral b `shiftL` 16
        .|. fromIntegral c `shiftL` 8
        .|. fromIntegral d
ipv4ListToWord32 _ = error "Invalid IPv4 list"
