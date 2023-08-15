module NopeTest where

import Data.Monoid
import Nope
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

nopeTests :: TestTree
nopeTests = testGroup "Nope" [functorTests, applicativeTests]

xs :: Nope (String, Sum Int, Product Int)
xs = NopeDotJpg

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
