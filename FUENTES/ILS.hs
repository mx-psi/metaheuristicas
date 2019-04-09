module ILS(ils) where
import Base
import Utils
import System.Random
import P1
import qualified Data.Vector.Unboxed as U
import Control.Monad(foldM)
import Data.List (nub)
import qualified Data.Vector.Unboxed.Mutable as M

ils :: StdGen -> Algorithm
ils g ds = getWs g $ (randSol ds >>= localIls ds) >>= (14 `timesM` ilsStep ds)

pickFeats :: Weights -> Int -> Rand [Int]
pickFeats ws n = take n . nub <$> randRs (0, U.length ws - 1)

-- | Vecino de un vector de pesos en ILS
vecinoILS :: Weights -> Int -> Rand Weights
vecinoILS v i = do
  sample <- randNormal
  let newval = min 1 $ max 0 $ v `U.unsafeIndex` i + 0.4 * sample
  pure (U.modify (\w -> M.unsafeWrite w i newval) v)

mutate :: DataSet -> Solution -> Rand Solution
mutate ds s = do
  feats <- pickFeats v (n `quot` 10)
  newv  <- foldM vecinoILS v feats
  sol ds newv
 where
  v = getV s
  n = U.length v

ilsStep :: DataSet -> Solution -> Rand Solution
ilsStep ds s = do
  reset
  s'  <- mutate ds s
  s'' <- localIls ds s'
  pure $ max s s''
