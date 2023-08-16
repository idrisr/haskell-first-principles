module ListTest where

import Data.Monoid
import List
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

listTests :: TestTree
listTests =
    testGroup
        "List"
        [ functorTests
        , applicativeTests
        , monadTests
        ]

xs :: List (String, Sum Int, Product Int)
xs = Cons ("", 1, 0) Nil

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
