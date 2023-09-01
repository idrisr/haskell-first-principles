module IPV6Test where

import IPV6
import Test.Tasty
import Text.Trifecta
import Util

ipv6tests :: TestTree
ipv6tests =
    testGroup
        "ipv6"
        [ segmentTests
        , segmentsTests
        , segmentLeadingColonTests
        ]

segmentTests :: TestTree
segmentTests = testGroup "Segment" [
    let sut = "0"
        wot = Just $ IP6Segment 0
        got = parseString parseSegment mempty sut
           in parseTestCase sut got wot
  , let sut = "ffff"
        wot = Just $ IP6Segment 65535
        got = parseString parseSegment mempty sut
           in parseTestCase sut got wot
  , let sut = "gg"
        wot = Nothing
        got = parseString parseSegment mempty sut
           in parseTestCase sut got wot
  , let sut = "11111"
        wot = Nothing
        got = parseString parseSegment mempty sut
           in parseTestCase sut got wot
  , let sut = "a"
        wot = Just $ IP6Segment 10
        got = parseString parseSegment mempty sut
           in parseTestCase sut got wot
  , let sut = ""
        wot = Just Placeholder
        got = parseString parseSegment mempty sut
           in parseTestCase sut got wot
    ]

segmentsTests :: TestTree
segmentsTests =
    let p = parseString parseSegments mempty
     in testGroup
            "Segments"
            [ let sut = "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
                  got = p sut
                  wot = Just $ fmap IP6Segment [65152, 0, 0, 0, 514, 46079, 65054, 33577]
               in parseTestCase sut got wot
            , let sut = "FFFF::0000"
                  -- "FFFF:_:0000"
                  got = p sut
                  wot = Just [IP6Segment 65535, Placeholder, IP6Segment 0]
               in parseTestCase sut got wot
            , let sut = "::FFFF:0000"
                  -- , let sut = "_ :_ :FFFF:0000"
                  got = p sut
                  wot = Just [Placeholder, IP6Segment 65535, IP6Segment 0]
               in parseTestCase sut got wot
            , let sut = "FFFF:0000::"
                  -- "FFFF:0000:_:_"
                  got = p sut
                  wot = Just [IP6Segment 65535, IP6Segment 0, Placeholder]
               in parseTestCase sut got wot
            , let sut = "::1"
                  got = p sut
                  wot = Just [Placeholder, IP6Segment 1]
               in parseTestCase sut got wot
            , let sut = "0000:0000"
                  got = p sut
                  wot = Just $ fmap IP6Segment [0, 0]
               in parseTestCase sut got wot
            , let sut = "0000:::0000"
                  got = p sut
                  wot = Just [IP6Segment 0, Placeholder, Placeholder, IP6Segment 0]
               in parseTestCase sut got wot
            ]

segmentLeadingColonTests :: TestTree
segmentLeadingColonTests =
    let p = parseString parseSegments mempty
     in testGroup
            "Leading Colon"
            [ let sut = "::FFFF:0000"
                  -- , let sut = "_ :_ :FFFF:0000"
                  got = p sut
                  wot = Just [Placeholder, IP6Segment 65535, IP6Segment 0]
               in parseTestCase sut got wot
            , let sut = "::1"
                  got = p sut
                  wot = Just [Placeholder, IP6Segment 1]
               in parseTestCase sut got wot
            ]

-- 0:0:0:0:0:ffff:ac10:fe01 -> 281473568538113
-- 0:0:0:0:0:ffff:cc78:f    -> 281474112159759

-- FE80:0000:0000:0000:0202:B3FF:FE1E:8329 -> 338288524927261089654163772891438416681
-- FE80::0202:B3FF:FE1E:8329

-- 2001:DB8::8:800:200C:417A -> 42540766411282592856906245548098208122
-- 2001:DB8::8:800:200C:417A
