module Genetic(genetic, toPop, generArStepMemetic) where
import Base
import P1
import Utils
import System.Random
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as U
import qualified Data.Set as S

genetic :: StdGen -> [(String, Algorithm)]
genetic g = [("Stationary BLX", stationBLX g), ("Stationary Arithmetic", stationAr g),
             ("Generational Arithmetic", generAr g), ("Generational BLX", generBLX g)]

-- | Muestrea de un conjunto
sample :: Population -> Rand Solution
sample pop = (`S.elemAt` pop) <$> randR (0, S.size pop -1)

-- | Torneo binario
duel :: Population -> Rand Solution
duel pop = maximum <$> replicateM size (sample pop)
  where size = 2

toPop :: DataSet -> [Weights] -> Rand Population
toPop ds xs = S.fromList <$> mapM (sol ds) xs

-- | Población inicial aleatoria
randPop :: Int -> DataSet -> Rand Population
randPop popSize ds = replicateM popSize (randWeights ds) >>= toPop ds

-- Algoritmos --

-- | El mejor de una población
getBest :: StdGen -> Rand Population -> Weights
getBest g s = getV $ S.findMax pop
 where pop = evalState s (g,0)

iterAlgo step g ds = getBest g $ untilM (\_ -> (> 15000) <$> gets snd) (step ds) (randPop 30 ds)

stationBLX = iterAlgo stationBLXStep
stationAr  = iterAlgo stationArStep
generBLX   = iterAlgo generBLXStep
generAr    = iterAlgo generArStep

-- -- | Random Mutate
mutStation :: [Weights] -> Rand [Weights]
mutStation = mapM (U.mapM mutateGen)
  where mutateGen x = do
          p <- randR (0,1) :: Rand Double
          if p < 0.001 then (do
            n <- randNormal
            return $ min 1 $ max 0 $ x + 0.3*n)
          else return x

-- | Añade dos hijos a una población y elimina los dos peores
contend ds cs pop = do
  children <- toPop ds cs
  return $ S.deleteMin $ S.deleteMin (children `S.union` pop)


-- | Cruce BLX
blx ::  Weights -> Weights -> Rand Weights
blx = U.zipWithM cr
  where cr x y = let i = abs (x - y) in
          (max 0 . min 1) <$> randR (min x y - i*alpha, max x y + i*alpha)
        alpha = 0.3

-- Estacionario BLX
stationBLXStep ds pop = do
    [p1, p2] <- replicateM 2 (getV <$> duel pop)
    children <- replicateM 2 (blx p1 p2)
    mutated  <- mutStation children
    contend ds mutated pop

-- | Cruce aritmético
arith :: Weights -> Weights -> Weights
arith = U.zipWith (\x y -> (x+y)*0.5)

-- Estacionario Aritmético
stationArStep ds pop = do
    [p1,p2,p3,p4] <- replicateM 4 (getV <$> duel pop)
    let children = [arith p1 p2, arith p3 p4]
    mutated  <- mutStation children
    contend ds mutated pop


-- -- Generacional BLX --

mutGener pop = do
  j <- randR (0, S.size pop -1)
  let old = S.elemAt j pop
  new <- Sol (-1) (getN old) <$> neighbor 0.3 (getV old)
  return $ S.insert new $ S.deleteAt j pop

crossBLX ss = replicateM 2 (Sol (-1) 0 <$> blx p1 p2)
  where [p1,p2] = map getV ss

buildPop ds mutPop = do
  evaluated <- S.fromList <$> mapM (sol ds . getV) (S.toList new)
  return $ old `S.union` evaluated
  where (new, old) = S.spanAntitone ((==) (-1) . getEval) mutPop

generBLXStep ds pop = do
  children  <- concat <$> replicateM nCross (replicateM 2 (duel pop) >>= crossBLX)
  unchanged <- replicateM (S.size pop - nCross*2) (duel pop)

  mutPop   <- (nMut `timesM` mutGener) (S.fromList $ children ++ unchanged)
  finalPop <- buildPop ds mutPop
  return (S.deleteMin $ S.insert best finalPop)
  where nMut   = (S.size pop)*(nFeats ds) `quot` 1000
        nCross = floor (fromIntegral (S.size pop)*0.7) `quot` 2
        best = S.findMax pop

crossAr :: [Solution] -> [Solution]
crossAr ss = map (Sol (-1) 0) [arith p1 p2, arith p2 p3, arith p3 p4, arith p4 p1]
  where [p1,p2,p3,p4] = map getV ss

generArStep ds pop = do
  children  <- concat <$> replicateM nCross (crossAr <$> replicateM 4 (duel pop))
  unchanged <- replicateM ((S.size pop) - nCross*4) (duel pop)

  mutPop   <- (nMut `timesM` mutGener) (S.fromList $ children ++ unchanged)
  finalPop <- buildPop ds mutPop
  return (S.deleteMin $ S.insert best finalPop)
  where nMut   = (S.size pop)*(nFeats ds) `quot` 1000
        nCross = floor (fromIntegral (S.size pop)*0.7) `quot` 4
        best = S.findMax pop

generArStepMemetic ds pop = do
  children  <- concat <$> replicateM nCross (crossAr <$> replicateM 4 (duel pop))
  unchanged <- replicateM (S.size pop - nCross*4) (duel pop)
  mutPop   <- (nMut `timesM` mutGener) (S.fromList $ children ++ unchanged)
  finalPop <- buildPop ds mutPop
  return (S.deleteMin $ S.insert best finalPop)
  where nMut   = (S.size pop)*(nFeats ds) `quot` 100
        nCross = 2
        best = S.findMax pop
