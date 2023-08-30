module PositiveInt where

import Text.Trifecta

parseDigit :: Parser Char
parseDigit = choice $ fmap char ['0'..'9']

base10Integer :: Parser Integer
base10Integer = do
    a <- some parseDigit
    return $ read a

base10Integer1 :: Parser Integer
base10Integer1 = do
    r <- option '+' $ char '-'
    n <- base10Integer
    let f '-' = -n
        f _ = n
    return $ f r
