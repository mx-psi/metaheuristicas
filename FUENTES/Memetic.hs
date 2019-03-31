{-# LANGUAGE BangPatterns #-}
module Memetic(memetic) where
import Base
import Genetic
import Utils
import P1
import System.Random
import Control.Monad.State.Strict
import qualified Data.Set as S

memetic :: StdGen -> [(String, Algorithm)]
memetic g =
  [ ("AM-(10,0.1mej)", memeticMej g)
  , ("AM-(10,0.1)"   , memeticRand g)
  , ("AM-(10,1)"     , memeticAll g)
  ]

startMem :: DataSet -> Rand (Int, Population)
startMem ds = do
  pop <- replicateM 10 (randWeights ds) >>= toPop ds
  pure (1, pop)

-- | El mejor de una población
getBest :: StdGen -> Rand (Int, Population) -> Weights
getBest g s = getV $ S.findMax pop where (_, pop) = evalState s (g, 0)

iterAlgo step g ds =
  getBest g $ untilM (\_ -> (> 15000) <$> gets snd) (step ds) (startMem ds)

memAll, memRand, memMej
  :: DataSet -> (Int, Population) -> Rand (Int, Population)

memeticAll = iterAlgo memAll
memAll ds (!n, pop) = (,) (n + 1) <$> if n `mod` 11 == 0
  then S.fromList <$> mapM (memeticLocal ds) (S.toList pop)
  else generArStepMemetic ds pop


localProb ds x = do
  p <- randR (0, 1) :: Rand Double
  if p < 0.1 then memeticLocal ds x else pure x

memeticRand = iterAlgo memRand
memRand ds (!n, pop) = (,) (n + 1) <$> if n `mod` 11 == 0
  then S.fromList <$> mapM (localProb ds) (S.toList pop)
  else generArStepMemetic ds pop

memeticMej = iterAlgo memMej
memMej ds (!n, pop) = (,) (n + 1) <$> if n `mod` 11 == 0
  then
    ( do
      let (m, pop') = S.deleteFindMax pop
      m' <- memeticLocal ds m
      pure $ S.insert m' pop'
    )
  else generArStepMemetic ds pop
