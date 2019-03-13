module SA(sa) where
import Base
import Utils
import P1(neighbor)
import System.Random
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as U

type Temperature = Double

-- | The type of a Solution
data SAData = SA {
  getSol     :: Solution,     -- | The current solution
  getBest    :: Solution,     -- | The best Solution so far
  getK       :: !Int,         -- | Number of coolings
  getT       :: !Temperature, -- | Current temperature
  nSuccess   :: !Int,         -- | Current number of succeses
  nNeighbors :: !Int,         -- | Current number of neighbors
  beta       :: Double        -- | Beta parameter
  }


--------------------
-- INICIALIZACIÓN --
--------------------

-- Temperatura inicial
getT0 :: Solution -> Temperature
getT0 s = mu*(50*getEval s)/(- log phi)
  where mu  = 0.3 :: Double
        phi = 0.3 :: Double

maxNeighbors s = 5*U.length (getV s) -- max vecinos
maxSuccess   s = maxNeighbors s `quot` 10

getBeta :: Solution -> Double -> Double
getBeta s t0 = (t0 - tf)/((15000/ fromIntegral (maxNeighbors s))*t0*tf)
  where tf = 10**(-3) :: Temperature -- Temperatura final

initial :: Solution -> SAData
initial s = SA {getSol = s, getBest = s, getK = 0,
             getT = getT0 s, nSuccess = -1, nNeighbors = 0,
             beta = getBeta s (getT0 s)}

--------------------
-- MODIFICACIONES --
--------------------

-- Cools temperature and increases number of coolings
cool :: SAData -> SAData
cool saData@(SA {getK = k, getT = cT, beta = b}) = saData {
  getK = k+1,
  getT = coolT b cT}
  where coolT betaC t = 0.95*t --t/(1 + betaC*t)

-- Updates current solution. Updates best if better. Increases number of sucesses
setCur :: Solution -> SAData -> SAData
setCur s saData@(SA {getBest = best, nSuccess = cS, nNeighbors = n}) = saData {
  getSol = s,
  getBest = max s best,
  nSuccess = cS + 1,
  nNeighbors = n + 1}

inc :: SAData -> SAData
inc saData@(SA {nNeighbors = n}) = saData {nNeighbors = n+1}

-- Reset number of succeses and Neighbors
resetC :: SAData -> SAData
resetC saData = saData {
  nSuccess = 0,
  nNeighbors = 0}


-------------------------
-- ALGORITMO PRINCIPAL --
-------------------------

sa :: StdGen -> Algorithm
sa g ds = getWs g $ getBest <$> untilM stopCondition (saStep ds) (initial <$> randSol ds)

stopCondition :: SAData -> Rand Bool
stopCondition saData = do
  nEvals <- gets snd
  return (nEvals >= 15000 || nSuccess saData == 0)


-- Paso del algoritmo
saStep :: DataSet -> SAData -> Rand SAData
saStep ds saData = do
  let reseted = resetC saData
  newSA <- exploreNeighbors ds reseted
  return (cool newSA)

exploreNeighbors :: DataSet -> SAData -> Rand SAData
exploreNeighbors ds = untilM checkStop (genNeighbor ds) . return
  where checkStop sd = return (nNeighbors sd > maxNeighbors (getSol sd) || nSuccess sd > maxSuccess (getSol sd))

genNeighbor :: DataSet -> SAData -> Rand SAData
genNeighbor ds sd = do
  s' <- neighbor 0.3 v >>= sol ds
  let dif = getEval s - getEval s'
  r <- randR (0,1) :: Rand Double
  if dif < 0 || r <= exp (-dif/(k*t)) then
    return (setCur s' sd)
   else
    return (inc sd)
 where s = getSol sd
       v = getV s
       k = fromIntegral (getK sd)
       t = getT sd
