module SemVerTest where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta
import SemVer
import Util

semVerTests :: TestTree
semVerTests = testGroup "Sem Ver" [unitTests]

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
