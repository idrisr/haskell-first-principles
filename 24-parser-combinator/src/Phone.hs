module Phone where

import Text.Trifecta

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber
    = PhoneNumber
        NumberingPlanArea
        Exchange
        LineNumber
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
    {- hlint ignore "Use <$>" -}
    n <- parseNPA
    e <- parseExchange
    l <- parseLN
    return $ PhoneNumber n e l

parseNPA :: Parser NumberingPlanArea
parseNPA = choice [pv3, pv2, pv1]

parseExchange :: Parser Exchange
parseExchange = pv1

parseLN :: Parser LineNumber
parseLN = do
    a <- choice $ fmap char ['0' .. '9']
    b <- choice $ fmap char ['0' .. '9']
    c <- choice $ fmap char ['0' .. '9']
    d <- choice $ fmap char ['0' .. '9']
    let e :: Int
        e = read [a, b, c, d]
    return e

pv1 :: Parser NumberingPlanArea
pv1 = do
    a <- choice $ fmap char ['1' .. '9']
    b <- choice $ fmap char ['0' .. '9']
    c <- choice $ fmap char ['0' .. '9']
    _ <- char '-'
    let d :: Int
        d = read [a, b, c]
    return d

pv2 :: Parser NumberingPlanArea
pv2 = do
    a <- parens integer
    return $ fromInteger a

pv3 :: Parser NumberingPlanArea
pv3 = do
    _ <- string "1-"
    pv1
