module EarlyTransTest where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.State

testsEarly :: TestTree
testsEarly =
    testGroup
        ""
        [ let
            got :: ((), ((), Integer))
            got = runStateT (put 8) 7
            wot :: ((), ((), Integer))
            wot = ((), ((), 8))
           in
            testCase "" $ got @?= wot
        ]

-- Prelude> (runStateT $ put 1 >> get) 0
-- (1,1)
-- Prelude> rs = runStateT
-- Prelude> n = 10021490234890
-- Prelude> (rs $ put 2 >> get) n
-- (2, 2)
-- Prelude> (rs $ put 2 >> return 9001) 0
-- (9001,2)
