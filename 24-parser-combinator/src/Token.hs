module Token where

import Text.Trifecta

tokP :: Parser [Integer]
tokP = some $ do
    i <- token $ some digit
    return $ read i
