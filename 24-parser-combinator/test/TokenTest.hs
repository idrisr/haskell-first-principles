module TokenTest where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta
import Token
import Util

tokenTests :: TestTree
tokenTests =
    testGroup
        "Token"
        [ unitTests
        , tokTests
        , scopeTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "how to get [123, 456]?"
        [ let sut = "123 456"
              p :: Parser String
              p = some digit
              got = parseString p mempty sut
              wot = Just "123"
           in testCase "some" $ maybeSuccess got @=? wot
        , let sut = "123 456"
              p :: Parser [String]
              p = some $ some digit
              got = parseString p mempty sut
              wot = Just ["123"]
           in testCase "some some" $ maybeSuccess got @=? wot
        , let sut = "123"
              p :: Parser [Integer]
              p = some integer
              got = parseString p mempty sut
              wot = Just [123]
           in testCase "some integer" $ maybeSuccess got @=? wot
        , let sut = "123456"
              p :: Parser [Integer]
              p = some integer
              got = parseString p mempty sut
              wot = Just [123456]
           in testCase "some integer" $ maybeSuccess got @=? wot
        , let sut = "123 456"
              p :: Parser [Integer]
              p = some integer
              got = parseString p mempty sut
              wot = Just [123, 456]
           in testCase "some integer" $ maybeSuccess got @=? wot
        , let sut = "123\n \n \n 456 \n"
              p :: Parser [Integer]
              p = some integer
              got = parseString p mempty sut
              wot = Just [123, 456]
           in testCase "some integer" $ maybeSuccess got @=? wot
        ]

tokTests :: TestTree
tokTests =
    testGroup
        "put a token on it?"
        [ let sut = "123\n \n 456"
              p :: Parser String
              p = token $ some digit
              got = parseString p mempty sut
              wot = Just "123"
           in testCase "token some integer" $ maybeSuccess got @=? wot
        , let sut = "123\n \n 456"
              p :: Parser [Integer]
              p = some decimal
              got = parseString p mempty sut
              wot = Just [123]
           in testCase "some decimal" $ maybeSuccess got @=? wot
        , let sut = "123\n \n 456"
              p :: Parser [Integer]
              p = some $ token decimal
              got = parseString p mempty sut
              wot = Just [123, 456]
           in testCase "some $ token decimal" $ maybeSuccess got @=? wot
        , let sut = "1\n2\n \n3"
              p :: Parser [Integer]
              p = some integer
              got = parseString p mempty sut
              wot = Just [1, 2, 3]
           in testCase "some integer" $ maybeSuccess got @=? wot
        , let sut = "1\n2\n \n3"
              p :: Parser [Integer]
              p = tokP
              got = parseString p mempty sut
              wot = Just [1, 2, 3]
           in testCase "tokP" $ maybeSuccess got @=? wot
        , let sut = "1\n2\n \n3"
              p :: Parser String
              p = token $ some digit
              got = parseString p mempty sut
              wot = Just "1"
           in testCase "tokP 2" $ maybeSuccess got @=? wot
        , let sut = "1\n2\n \n3"
              p :: Parser [String]
              p = some $ token $ some digit
              got = parseString p mempty sut
              wot = Just ["1", "2", "3"]
           in testCase "tokP 2" $ maybeSuccess got @=? wot
        ]

scopeTests :: TestTree
scopeTests =
    testGroup
        "scope"
        $ let
            tknWhole :: Parser Char
            tknWhole = token $ char 'a' >> char 'b'
           in
            [ let sut = "a b"
                  p :: Parser Char
                  p = tknWhole
                  got = parseString p mempty sut
                  wot = Nothing
               in testCase "" $ maybeSuccess got @=? wot
            , let sut = "ab"
                  p :: Parser Char
                  p = tknWhole
                  got = parseString p mempty sut
                  wot = Just 'b'
               in testCase "" $ maybeSuccess got @=? wot
            , let sut = " ab"
                  p :: Parser Char
                  p = tknWhole
                  got = parseString p mempty sut
                  wot = Nothing
               in testCase "" $ maybeSuccess got @=? wot
            , let sut = "ab ab"
                  p :: Parser Char
                  p = tknWhole
                  got = parseString p mempty sut
                  wot = Just 'b'
               in testCase "" $ maybeSuccess got @=? wot
            , let sut = "ab ab"
                  p :: Parser String
                  p = some tknWhole
                  got = parseString p mempty sut
                  wot = Just "bb"
               in testCase "" $ maybeSuccess got @=? wot
            , let sut = "b ab"
                  p :: Parser String
                  p = some tknWhole
                  got = parseString p mempty sut
                  wot = Nothing
               in testCase "" $ maybeSuccess got @=? wot
            ] ++
        let
            tknCharA :: Parser Char
            tknCharA = token (char 'a') >> char 'b'
           in [
             let sut = "a b"
                 p :: Parser Char
                 p = tknCharA
                 got = parseString p mempty sut
                 wot = Just 'b'
               in testCase "" $ maybeSuccess got @=? wot
            ,let sut = "a ba b"
                 p :: Parser String
                 p = some tknCharA
                 got = parseString p mempty sut
                 wot = Just "bb"
               in testCase "" $ maybeSuccess got @=? wot
            ,let sut = "a b a b"
                 p :: Parser String
                 p = some tknCharA
                 got = parseString p mempty sut
                 wot = Just "b"
               in testCase "" $ maybeSuccess got @=? wot
           ]
