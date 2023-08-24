{-# LANGUAGE OverloadedStrings #-}

module Data.IniTest where

import Data.ByteString (ByteString)
import Data.Ini
import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta
import Util

iniTests :: TestTree
iniTests =
    testGroup
        "Ini"
        [ assnTests
        , headerTests
        , commentTests
        , sectionTests
        ]

assnTests :: TestTree
assnTests = testGroup "Assignment" [
    let
        got = parseByteString parseAssignment mempty assignmentEx
        wot :: Maybe (Name, Value)
        wot = Just ("woot", "1")
        in
    testCase "single assn" $ maybeSuccess got @?= wot
  , let
        sut :: ByteString
        sut = "key=value"
        got = parseByteString parseAssignment mempty sut
        wot :: Maybe (Name, Value)
        wot = Just ("key", "value")
        in
    testCase "single assn" $ maybeSuccess got @?= wot
  , let
        sut :: ByteString
        sut = "key=value\nkey=value"
        got = parseByteString (some parseAssignment) mempty sut
        wot :: Maybe [(Name, Value)]
        wot = Just [("key", "value"), ("key", "value")]
        in
    testCase "double assn" $ maybeSuccess got @?= wot
    ]

headerTests :: TestTree
headerTests = testGroup "Header" [
    let
        got = parseByteString parseHeader mempty headerEx
        wot :: Maybe Header
        wot = Just (Header "blah")
        in
    testCase "" $ maybeSuccess got @?= wot
    ]

commentTests :: TestTree
commentTests = testGroup "Comments" [
    let
        p :: Parser Header
        p = skipComments >> parseHeader
        sut = "; woot\n[blah]"
        got = parseByteString p mempty sut
        wot :: Maybe Header
        wot = Just (Header "blah")
        in
    testCase "Skip Comments before header" $ maybeSuccess got @?= wot
    ]

sectionTests :: TestTree
sectionTests = testGroup "Section" [
    let
        got = parseByteString parseSection mempty sectionEx
        states = M.fromList [("Chris", "Texas")]
        wot :: Maybe Section
        wot = Just (Section (Header "states") states)
        in
    testCase "single section" $ maybeSuccess got @?= wot
  , let
        got = parseByteString parseIni mempty sectionEx2
        sectionValues =
            M.fromList
            [ ("alias", "claw")
            , ("host", "wikipedia.org")]
        whatisitValues =
            M.fromList
            [("red", "intoothandclaw")]
        wot :: Maybe Config
        wot = Just (Config
            (M.fromList
            [ (Header "section"
            , sectionValues)
            , (Header "whatisit"
            , whatisitValues)]))
        in
    testCase "multiple section" $ maybeSuccess got @?= wot
    ]
