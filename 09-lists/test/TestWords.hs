module TestWords where

import Test.QuickCheck (Arbitrary (..), choose, quickCheck)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (==>))
import Words

wordTests :: TestTree
wordTests = testGroup "words" [testWords, testLines]

testWords :: TestTree
testWords =
    testGroup
        "word split"
        [ testCase "" $ myWords "sheryl wants fun" @?= ["sheryl", "wants", "fun"]
        ]

testLines :: TestTree
testLines =
    testGroup
        "word split"
        [ testCase "" $ myLines sentences @?= ["Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]
        ]
