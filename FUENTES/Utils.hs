module Utils(
  Rand
  ,randR
  ,randRs
  ,randNormal
  ,randWeights
  ,untilM
  ,incEval
  ,reset
  ,sol
  ,getWs
  ,randSol
  ,timesM
  ,splitGroups
  ,(+:+)
  ,both) where

import System.Random
import Data.Function(on)
import Base
import KNN(rawEval)
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as U
import Data.Random.Normal(normal)

----------------
-- RANDOMNESS --
----------------

-- A random value
type Rand a = State (StdGen,Int) a

---------------------------
-- CREATE A NEW SOLUTION --
---------------------------

addEval :: Int -> Rand ()
addEval n = modify (fmap (+ n))

reset :: Rand ()
reset = modify (fmap (const 0))

-- | Increment eval and get the number
incEval :: Rand ()
incEval = addEval 1

sol :: DataSet -> Weights -> Rand Solution
sol ds v = do
  incEval
  n <- gets snd
  pure (Sol (rawEval v ds) n v)

----------------------
-- RANDOM FUNCTIONS --
----------------------

toRandC :: (StdGen -> (a, StdGen)) -> Rand a
toRandC r = state $ \(g, n) -> let (a, g') = r g in (a, (g', n))

-- | Generate random value within bounds
randR :: (Random a) => (a, a) -> Rand a
randR = toRandC . randomR

randRs :: (Random a) => (a, a) -> Rand [a]
randRs (a, b) = randomRs (a, b) <$> splitS

chooseR :: Double -> Rand Bool
chooseR thr = (< thr) <$> (randR (0, 1) :: Rand Double)

splitS = toRandC split

-- | Generate random weights for a given dataset
randWeights :: DataSet -> Rand Weights
randWeights ds = U.fromList <$> (take (nFeats ds) . randomRs (0, 1) <$> splitS)

randNormal :: Rand Double
randNormal = toRandC normal

-------------------------------
-- MONADIC LOOPS AND GETTERS --
-------------------------------

getWs :: StdGen -> Rand Solution -> Weights
getWs g s = getV $ evalState s (g, 0)

randSol :: DataSet -> Rand Solution
randSol ds = do
  g <- gets fst
  sol ds (U.fromList $ take (nFeats ds) $ randoms g)

-- Repeat until a condition is fulfilled
untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> m a -> m a
untilM p f = goM
 where
  goM m = do
    x <- m
    b <- p x
    if b then m else goM (f x)

timesM n f = foldr (>=>) pure (replicate n f)

--------------------
-- List utilities --
--------------------

(//) :: Int -> Int -> Double
(//) = (/) `on` fromIntegral

-- | Evenly divides 'xs' in 'n' groups
splitGroups :: Int -> [a] -> [[a]]
splitGroups n xs = splitGroups' xs
 where
  m = ceiling $ length xs // n
  splitGroups' [] = []
  splitGroups' ys | length ys < m = [ys]
                  | otherwise     = take m ys : splitGroups' (drop m ys)


---------------------
-- Tuple utilities --
---------------------

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

(+:+) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) +:+ (c, d) = (a + c, b + d)
