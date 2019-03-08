{-# LANGUAGE BangPatterns #-}
module P1(local, memeticLocal, localIls, p1Algos, neighbor) where
import Base
import KNN
import System.Random
import Data.List.Zipper
import GHC.Exts(groupWith)
import qualified Data.Vector.Unboxed as U
import Data.List (minimumBy)
import Data.Function(on)
import Utils
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.State.Strict

p1Algos :: StdGen -> [(String, Algorithm)]
p1Algos g = [("1NN", one), ("Zero", zero), ("Random", uniform g),
                ("RELIEF", relief), ("Local", local g)]

----------------
-- BASE CASES --
----------------

-- | Zero case (sanity check)
zero, one :: Algorithm
zero      ds = U.replicate (nFeats ds) 0
one       ds = U.replicate (nFeats ds) 1

uniform :: StdGen -> Algorithm
uniform g ds = U.fromList $ take (nFeats ds) $ randoms g


-------------------------------
-- RELIEF ALGORITHM (GREEDY) --
-------------------------------

-- Normalizes vector
-- (Auxiliary function for relief)
normalize :: Weights -> Weights
normalize ws = U.map (\x -> if x < 0 then 0 else x/wm) ws
  where wm = U.maximum ws

-- Calculates d1 distance between vectors
-- (Auxiliary function for relief)
d1 :: Weights -> Weights -> Weights
d1 = U.zipWith (\x y -> abs (x-y))

closestTo :: [Point] -> Point -> Point
closestTo ds p = minimumBy (compare `on` distW ws p) ds
  where ws = U.replicate (U.length $ head ds) 1

-- Main loop of RELIEF algorithm
-- (Auxiliary function for relief)
diffs :: [Point] -> Zipper Point -> Weights
diffs enemies = foldrz delta (U.replicate (U.length $ head enemies) 0)
  where delta z !acc = U.zipWith3 (\a b c -> a+b-c) acc (enemy z `d1` cursor z) (friend z `d1` cursor z)
        friend z = closestTo (toList $ delete z) (cursor z)
        enemy  z = closestTo enemies (cursor z)

-- | RELIEF greedy algorithm for learning weights
-- Only works with two classes
relief :: Algorithm
relief ds = normalize $ U.zipWith (+) (diffs bs (fromList as)) (diffs as (fromList bs))
  where [as,bs] = map (map getFeats) $ groupWith getClass (getData ds)



-----------------------------
-- FIRST-BEST LOCAL SEARCH --
-----------------------------

-- | Vecino de un vector de pesos
neighbor :: Double -> Weights -> Rand Weights
neighbor sigma v = do
  sample <- randNormal
  i <- randR (0, U.length v -1)
  let newval = min 1 $ max 0 $ v `U.unsafeIndex` i + sigma*sample
  return (U.modify (\w -> M.unsafeWrite w i newval) v)

-- | Given a Solution, get the next Solution
localStep ::  DataSet -> Solution -> Rand Solution
localStep ds current = do
  new <-  neighbor 0.3 (getV current) >>= sol ds
  return (max new current)

cur = gets snd

-- | First best local search
local :: StdGen -> Algorithm
local g ds = getWs g $
  untilM (\s -> (> min 15000 (getN s + 20*numFeats)) <$> cur)
        (localStep ds)
        (randSol ds)
  where numFeats = nFeats ds


  ------------------------------------------------------
  -- Búsquedas locales auxiliares de otros algoritmos --
  ------------------------------------------------------

memeticLocal :: DataSet -> Solution -> Rand Solution
memeticLocal ds s = untilM (\_ -> (> 2*numFeats) <$> cur) (localStep ds) (return s)
  where numFeats = nFeats ds

localIls :: DataSet -> Solution -> Rand Solution
localIls ds s = untilM (\c -> (> min 1000 (getN c + 20*numFeats)) <$> cur) (localStep ds) (return s)
  where numFeats = nFeats ds
