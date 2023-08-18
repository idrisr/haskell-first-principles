module MoiTest where

import Data.Monoid
import Test.Tasty
import Moi

listTests :: TestTree
listTests =
    testGroup
        "List"
        [ functorTests
        , applicativeTests
        , monadTests
        ]

type A = (String, Sum Int, Product Int)
type S = (Sum Int, Product Int)
xs :: Moi S A
xs = undefined

functorTests :: TestTree
functorTests = undefined
    -- let props = unbatch $ functor xs
     -- in testGroup "Functor" [ testProperties "" props ]

applicativeTests :: TestTree
applicativeTests = undefined
    -- let props = unbatch $ applicative xs
     -- in testGroup "Applicative" [ testProperties "" props ]

monadTests :: TestTree
monadTests = undefined
    -- let props = unbatch $ monad xs
     -- in testGroup "Monad" [testProperties "" props]
