module TestOptional (optionalTests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Properties

optionalTests :: TestTree
optionalTests = testGroup "monoid" [assocTest, idTest]

assocTest :: TestTree
assocTest =
    testGroup
        "associativity"
        [ QC.testProperty "string" $ \a b c -> propSemiAssoc (a :: String) b c
        , QC.testProperty "maybe string" $ \a b c -> propSemiAssoc (a :: (Maybe String)) b c
        , QC.testProperty "list int" $ \a b c -> propSemiAssoc (a :: [Int]) b c
        , QC.testProperty "list string" $ \a b c -> propSemiAssoc (a :: [String]) b c
        ]

idTest :: TestTree
idTest =
    testGroup
        "identity"
        [ QC.testProperty "string right" $ \a -> propMonoidRightIdentity (a :: String)
        , QC.testProperty "string left" $ \a -> propMonoidLeftIdentity (a :: String)
        , QC.testProperty "list right" $ \a -> propMonoidRightIdentity (a :: [Int])
        , QC.testProperty "list left" $ \a -> propMonoidLeftIdentity (a :: [Int])
        ]
