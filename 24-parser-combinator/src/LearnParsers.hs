module LearnParsers where

import Text.Trifecta
import Control.Applicative

stop :: Parser a
stop = unexpected "stop"

wot :: Parser String
wot = string "1" <|> string "12" <|> string "123"

one :: Parser Char
one = char '1'

one1 :: Parser b
one1 = one >> stop

one2 :: Parser ()
one2 = char '1' >> eof

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo1 :: Parser b
oneTwo1 = oneTwo >> stop

oneTwoThree :: Parser String
oneTwoThree = string "1" >> string "2"

p123a :: Parser String
p123a = choice [string "123", string "12", string "1"]

p123b :: Parser String
p123b = some $ choice [char '1', char '2', char '3']

testParse :: Show a => Parser a -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

p123 :: Show a => Parser a -> String -> IO ()
p123 p s= print $ parseString p mempty s

things :: IO ()
things = do
    pNL "stop:"
    testParse (stop :: Parser ())

    pNL "one:"
    testParse one

    pNL "one':"
    testParse (one1 :: Parser Char)

    pNL "oneTwo:"
    testParse oneTwo

    pNL "oneTwo':"
    testParse (oneTwo1 :: Parser Char)

    pNL "one2':"
    testParse (one2 :: Parser ())

    -- pNL "oneTwoThree:"
    -- testParse (oneTwoThree :: Parser ())
