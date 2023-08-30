module SemVerTest (semVerTests) where

import SemVer
import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta
import Util

semVerTests :: TestTree
semVerTests =
    testGroup
        "Sem Ver"
        [ unitTests
        , mmpTests
        , releaseTests
        , metadataTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "unit"
        [ let sut = "2.1.1"
              got = parseString parseSemVer mempty sut
              wot =
                Just $
                    SemVer
                        2
                        1
                        1
                        []
                        []
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "1.0.0-x.7.z.92"
              got = parseString parseSemVer mempty sut
              wot =
                Just $
                    SemVer
                        1
                        0
                        0
                        [ NOSS "x"
                        , NOSI 7
                        , NOSS "z"
                        , NOSI 92
                        ]
                        []
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "1.0.0-gamma+002"
              got = parseString parseSemVer mempty sut
              wot =
                Just $
                    SemVer
                        1
                        0
                        0
                        [NOSS "gamma"]
                        [NOSI 2]
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "1.0.0-beta+oof.sha.41af286"
              got = parseString parseSemVer mempty sut
              wot =
                Just $
                    SemVer
                        1
                        0
                        0
                        [NOSS "beta"]
                        [ NOSS "oof"
                        , NOSS "sha"
                        , NOSS "41af286"
                        ]
           in testCase "" $ maybeSuccess got @?= wot
        ]

releaseTests :: TestTree
releaseTests =
    testGroup
        "release"
        [ let sut = "-"
              got = parseString parseRelease mempty sut
              wot = Just []
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "-x.7.z.92"
              got = parseString parseRelease mempty sut
              wot = Just [ NOSS "x"
                        , NOSI 7
                        , NOSS "z"
                        , NOSI 92
                        ]
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "-gamma"
              got = parseString parseRelease mempty sut
              wot = Just [NOSS "gamma"]
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "-beta"
              got = parseString parseRelease mempty sut
              wot = Just [NOSS "beta"]
           in testCase "" $ maybeSuccess got @?= wot
        ]

mmpTests :: TestTree
mmpTests =
    testGroup
        "mmp"
        [ let sut = "2.1.1"
              got = parseString parseMMP mempty sut
              wot = Just $ (,,) 2 1 1
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "1.0.0-x.7.z.92"
              got = parseString parseMMP mempty sut
              wot = Just $ (,,) 1 0 0
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "1.0.0-gamma+002"
              got = parseString parseMMP mempty sut
              wot = Just $ (,,) 1 0 0
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "1.0.0-beta+oof.sha.41af286"
              got = parseString parseMMP mempty sut
              wot = Just $ (,,) 1 0 0
           in testCase "" $ maybeSuccess got @?= wot
        ]

metadataTests :: TestTree
metadataTests =
    testGroup
        "metadata"
        [ let sut = "+"
              got = parseString parseMetadata mempty sut
              wot = Just []
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "+"
              got = parseString parseMetadata mempty sut
              wot = Just []
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "+002"
              got = parseString parseMetadata mempty sut
              wot = Just [NOSI 2]
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "+oof.sha.41af286"
              got = parseString parseMetadata mempty sut
              wot =
                Just
                    [ NOSS "oof"
                    , NOSS "sha"
                    , NOSS "41af286"
                    ]
           in testCase "" $ maybeSuccess got @?= wot
        ]
