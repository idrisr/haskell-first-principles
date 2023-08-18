module Starter where

import System.Random

a :: Int -> StdGen
a = mkStdGen

c :: (RandomGen g, Random a) => g -> (a, g)
c = random
