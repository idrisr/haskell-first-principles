module IdentityTest where

import Data.Monoid
import Identity
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

identityTests :: TestTree
identityTests =
    testGroup
        "Identity"
        [ functorTests
        , applicativeTests
        , monadTests
        ]

xs :: Identity (String, Sum Int, Product Int)
xs = Identity ("", 1, 0)

functorTests :: TestTree
functorTests =
    let props = unbatch $ functor xs
     in testGroup "Functor" [ testProperties "" props ]

applicativeTests :: TestTree
applicativeTests =
    let props = unbatch $ applicative xs
     in testGroup "Applicative" [ testProperties "" props ]

monadTests :: TestTree
monadTests =
    let props = unbatch $ monad xs
     in testGroup "Monad" [testProperties "" props]
