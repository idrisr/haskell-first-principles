{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ

type NumberOrString = Either Integer String

a :: String
a = "blah"

b :: String
b = "123"

c :: String
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

parseNos1 :: Parser NumberOrString
parseNos1 =
    skipMany (oneOf "\n")
    >>
    (Left <$> integer)
    <|> (Right <$> some letter)

eitherOr :: String
eitherOr = [r|
123

abc
456
def
|]

eitherOr1 :: String
eitherOr1 = [r|123

abc
456
def
|]

main :: IO ()
main = do
    let p f = parseString f mempty
    print $ p (some letter) a
    print $ p integer b
    print $ p parseNos a
    print $ p parseNos b
    print $ p (many parseNos) c
    print $ p (some parseNos) c
