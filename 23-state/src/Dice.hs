module Dice where

import Control.Applicative (liftA3)
-- import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Prelude hiding (sum)

data Die
    = DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    let s = mkStdGen 420
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, _) = randomR (1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie1 :: State StdGen Die
rollDie1 = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes1 :: State StdGen (Die, Die, Die)
rollDieThreeTimes1 = liftA3 (,,) rollDie rollDie rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
        | sum >= 20 = count
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go
                    (sum + die)
                    (count + 1)
                    nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
        | sum >= n = count
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go
                    (sum + die)
                    (count + 1)
                    nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, [])
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum count gen
        | sum >= n = count
        | otherwise =
            let (int, nextGen) = randomR (1, 6) gen
                die = intToDie int in
             go (sum + int) (fst count + 1, die : snd count) nextGen
