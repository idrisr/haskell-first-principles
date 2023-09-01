module Main where

import Text.Trifecta
import Data.Word (Word16)
import Data.List (intersperse)
import Numeric (readHex)

-- Define a data type to represent IPv6 addresses
data IPv6Address = IPv6Address [Word16]
    deriving Show

-- Parse a single hexadecimal group (e.g., "abcd")
hexGroupParser :: Parser Word16
hexGroupParser = do
    hexStr <- some hexDigit
    case readHex hexStr of
        [(value, _)] -> return value
        _            -> fail "Invalid hexadecimal group"

-- Parse an IPv6 address
ipv6AddressParser :: Parser IPv6Address
ipv6AddressParser = do
    groups <- sepBy hexGroupParser (char ':')
    let numGroups = length groups
    if numGroups == 8
        then return $ IPv6Address groups
        else if "::" `elem` groups
            then case split "::" groups of
                (before, after) -> do
                    let missingGroups = 8 - (length before + length after)
                        zeroGroups = replicate missingGroups 0
                    return $ IPv6Address $ before ++ zeroGroups ++ after
            else fail "Invalid IPv6 address"

-- Helper function to split a list into two parts based on a delimiter
split :: Eq a => a -> [a] -> ([a], [a])
split delimiter list = case break (== delimiter) list of
    (before, [])  -> (before, [])
    (before, after) -> (before, tail after)

main :: IO ()
main = do
    let input = "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
    case parseString ipv6AddressParser mempty input of
        Success ipv6Address -> putStrLn $ "Parsed IPv6 Address: " ++ show ipv6Address
        Failure err         -> putStrLn $ "Parsing failed: " ++ show err
