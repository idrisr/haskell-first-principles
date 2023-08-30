module IPV4Test where

import IPV4
import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta
import Util

ipv4tests :: TestTree
ipv4tests =
    testGroup
        "ipv4"
        [ segmentTests
        , segmentsTests
        , addressTests
        ]

segmentTests :: TestTree
segmentTests = testGroup "Segment" [
    let sut = "0"
        wot = Just 0
        got = parseString parseSegment mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "00"
        wot = Nothing
        got = parseString parseSegment mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "000"
        wot = Nothing
        got = parseString parseSegment mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "01"
        wot = Nothing
        got = parseString parseSegment mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "256"
        wot = Nothing
        got = parseString parseSegment mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "255"
        wot = Just 255
        got = parseString parseSegment mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "25"
        wot = Just 25
        got = parseString parseSegment mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "2"
        wot = Just 2
        got = parseString parseSegment mempty sut in
        testCase sut $ maybeSuccess got @?= wot
    ]

segmentsTests :: TestTree
segmentsTests = testGroup "Segments" [
    let sut = "0.1.1.1"
        wot = Just [0, 1, 1, 1]
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "0.1.1.1."
        wot = Nothing
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "0.1.1.1"
        wot = Just [0, 1, 1, 1]
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "256.0.0.1"
        wot = Nothing
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "254.0.0.1"
        wot = Just [254, 0, 0, 1]
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "192.168.01.1"
        wot = Nothing
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "192.168.1.1"
        wot = Just [192, 168, 1, 1]
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "192.168..1"
        wot = Nothing
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "192.168.69.1"
        wot = Just [192, 168, 69, 1]
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "192.168.1.1."
        wot = Nothing
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "192.168.1.1"
        wot = Just [192, 168, 1, 1]
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "192. 168.1.1"
        wot = Nothing
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
  , let sut = "192.168.1.1"
        wot = Just [192, 168, 1, 1]
        got = parseString parseSegments mempty sut in
        testCase sut $ maybeSuccess got @?= wot
    ]

addressTests :: TestTree
addressTests =
    testGroup
        "Segments"
        [ let sut = "172.16.254.1"
              wot = Just $ IPAddress 2886794753
              got = parseString parseIPV4 mempty sut
           in testCase sut $ maybeSuccess got @?= wot
        , let sut = "204.120.0.15"
              wot = Just $ IPAddress 3430416399
              got = parseString parseIPV4 mempty sut
           in testCase sut $ maybeSuccess got @?= wot
        , let sut = "204.120.0.15.0"
              wot = Nothing
              got = parseString parseIPV4 mempty sut
           in testCase sut $ maybeSuccess got @?= wot
        , let sut = "0.15.0"
              wot = Nothing
              got = parseString parseIPV4 mempty sut
           in testCase sut $ maybeSuccess got @?= wot
        ]
