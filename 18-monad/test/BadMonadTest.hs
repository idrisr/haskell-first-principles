module BadMonadTest where

import BadMonad
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Checkers
import Data.Monoid

badMonadTests :: TestTree
badMonadTests = testGroup "BadMonad" [functorTests, applicativeTests, monadTests]

xs :: CountMe (String, Sum Int, Product Int)
xs = CountMe 10 a
    where a = ("YO", 10, 69)

functorTests :: TestTree
functorTests =
    let props = unbatch $ functor xs
     in testGroup "Functor" [QC.testProperties "" props]

applicativeTests :: TestTree
applicativeTests =
    let props = unbatch $ applicative xs
     in testGroup "Applicative" [QC.testProperties "" props]

monadTests :: TestTree
monadTests =
    let props = unbatch $ monad xs
     in testGroup "Monad" [QC.testProperties "" props]
