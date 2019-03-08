module DE(de) where
import Base
import Utils
import KNN
import System.Random
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.List(nub)

--------------------
-- |TIPOS DE DATOS --
--------------------

-- |Tipo de solución específico para DE; igual que Solution pero puede comparar por igualdad
data DESolution = DESol {deEval :: !Double, deV :: Weights} deriving Eq

deSol :: DataSet -> Weights -> Rand DESolution
deSol ds v = incEval >> return (DESol (rawEval v ds) v)

instance Ord DESolution where
  (DESol x _ ) <= (DESol y _) = x <= y
  max new current = if deEval new > deEval current then new else current

-- |Tipo de población específico para DE
type DEPopulation = V.Vector DESolution

-- | Toma `n` elementos de `pop` distintos de `i`
pickDistinctFrom :: DEPopulation -> Int -> Int -> Rand [DESolution]
pickDistinctFrom pop i n = map (\k -> (pop V.! k)) <$> indices
  where indices = (take n . (filter (/= i)) . nub) <$> randRs (0, V.length pop - 1)

----------------------------------
-- |GESTIÓN DE BUCLE Y POBLACIÓN --
----------------------------------

-- |Algoritmos de DE
de :: StdGen -> [(String, Algorithm)]
de g = [("DE Rand", deRand g), ("DE Best", deBest g)]

-- |Condición de parada
stopCondition :: DEPopulation -> Rand Bool
stopCondition _ = (> 15000) <$> gets snd

-- |Consigue el mejor de una población aleatoria
getBest :: StdGen -> Rand DEPopulation -> Weights
getBest g s = deV $ maximum pop
 where pop = evalState s (g,0)

-- |Pesos aleatorios
toDEPop :: DataSet -> [Weights] -> Rand DEPopulation
toDEPop ds xs = V.fromList <$> mapM (deSol ds) xs

-- | Población inicial aleatoria
randDEPop :: Int -> DataSet -> Rand DEPopulation
randDEPop popsize ds = replicateM popsize (randWeights ds) >>= toDEPop ds

-- |DE/Rand/1
deRand, deBest :: StdGen -> Algorithm
deRand g ds = getBest g $ untilM stopCondition (deRandStep ds) (randDEPop 50 ds)
deBest g ds = getBest g $ untilM stopCondition (deBestStep ds) (randDEPop 50 ds)

-------------------------------
-- |PARTE PRINCIPAL ALGORITMO --
-------------------------------

-- | Cruce coordenada a coordenada rand
crossDeRand :: [Weights] -> Int -> Int -> Double -> Rand Double
crossDeRand neighbors jrand i xi = do
  let [a,b,c] = map (U.! i) neighbors :: [Double]
  ri <- randR (0,1) :: Rand Double

  if ri < cr || i == jrand then
    return $ min 1 $ max 0 $ a + f*(b-c)
   else
    return xi

  where cr = 0.5 :: Double
        f  = 0.5 :: Double

-- | Actualiza un miembro concreto (`s`) de la población
updateRand :: DataSet -> DEPopulation -> Int -> Int -> DESolution -> Rand DESolution
updateRand ds pop jrand i s = do
  neighbors <- map deV <$> pickDistinctFrom pop i 3 :: Rand [Weights]
  newV <- U.imapM (crossDeRand neighbors jrand) (deV s) :: Rand Weights
  new <- deSol ds newV :: Rand DESolution
  return (max new s)


deRandStep :: DataSet -> DEPopulation -> Rand DEPopulation
deRandStep ds pop = do
  jrand <- randR (0, nFeats ds - 1)
  V.imapM (updateRand ds pop jrand) pop


------------------
-- VERSIÓN BEST --
------------------

deBestStep :: DataSet -> DEPopulation -> Rand DEPopulation
deBestStep ds pop = do
  jrand <- randR (0, nFeats ds - 1)
  let best = deV (V.maximum pop)
  V.imapM (updateBest ds pop jrand best) pop


-- | Actualiza un miembro concreto (`s`) de la población
updateBest :: DataSet -> DEPopulation -> Int -> Weights -> Int -> DESolution -> Rand DESolution
updateBest ds pop jrand best i s = do
  neighbors <- map deV <$> pickDistinctFrom pop i 2 :: Rand [Weights]
  newV <- U.imapM (crossDeBest neighbors jrand best) (deV s) :: Rand Weights
  new <- deSol ds newV :: Rand DESolution
  return (max new s)

-- | Cruce coordenada a coordenada rand
crossDeBest :: [Weights] -> Int -> Weights -> Int -> Double -> Rand Double
crossDeBest neighbors jrand best i xi = do
  let [a,b] = map (U.! i) neighbors :: [Double]
  let xbest = best U.! i :: Double
  ri <- randR (0,1) :: Rand Double

  if ri < cr || i == jrand then
    return $ min 1 $ max 0 $ xi + f*(xbest-xi) + f*(a-b)
  else
    return xi

  where cr = 0.5 :: Double
        f  = 0.5 :: Double
