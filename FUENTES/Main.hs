module Main where

import Base
import Utils
import Read
import KNN
import System.Random(getStdGen,StdGen)
import System.Directory(listDirectory)
import Control.Monad(forM_)
import System.Clock(getTime,Clock(Monotonic),toNanoSecs)
import Data.List.Zipper
import Text.Printf
import GHC.Exts(groupWith)
import System.Random.Shuffle(shuffle')
import System.Environment

import SA
import ILS
import DE

main :: IO ()
main = do
  args <- getArgs

  if length args <= 1
    then
      ( do
        putStrLn "Ejecutando en todos los datasets disponibles"
        ds <- fmap (map ("Data/" ++)) (listDirectory "Data")
        if null args
          then
            ( do
              g <- getStdGen
              putStrLn $ "Usando el generador: \"" ++ show g ++ "\"\n"
              forM_ ds (testDataSet g)
            )
          else forM_ ds (testDataSet (read $ head args))
      )
    else forM_ (tail args) (testDataSet (read $ head args))

testDataSet :: StdGen -> FilePath -> IO ()
testDataSet g file = do
  putStrLn $ "Dataset: " ++ file
  dataSet <- readExamples file
  let chunks = cv g 5 dataSet
  forM_ ([("ILS", ils g), ("SA 5n Proporcional", sa g)] ++ de g) (stats chunks)

stats :: [DataSet] -> (String, Algorithm) -> IO ()
stats chunks (name, algo) = do
  putStrLn $ " Algoritmo: " ++ name
  let evChunks = foldrz (chunkStats algo) [] (fromList chunks)
  putStrLn "  Prec, Simpl,  Aggr, Tiempo"
  sequence_ evChunks

printEv :: (Double, Double) -> IO ()
printEv (p, s) = printf "%6.2f,%6.2f,%6.2f" p s (alpha * p + (1 - alpha) * s)
  where alpha = 0.5

chunkStats :: Algorithm -> Zipper DataSet -> [IO ()] -> [IO ()]
chunkStats algo z xs =
  ( do
      let rest = concatMap getData $ toList $ delete z
      let ws   = algo (length rest, rest)
      t1 <- getTime Monotonic
      printEv (eval ws rest (getData $ cursor z))
      t2 <- getTime Monotonic
      let diff = fromIntegral (toNanoSecs (t2 - t1)) * 10 ** (-9 :: Double)
      printf ",%7.4f\n" diff
    )
    : xs


-- | @cv gen n ps@ Splits 'ps' into 'n' fair subsets for cross-validation
cv :: StdGen -> Int -> Examples -> [DataSet]
cv gen n ps = shuf $ foldr1 (zipWith (++)) $ map (splitGroups n) shuffled
 where
  shuf     = map (\chunk -> (length chunk, shuffle' chunk (length chunk) gen))
  groups   = groupWith getClass ps -- points grouped by class :: Examples
  lengths  = map length groups
  shuffled = zipWith (\l g -> shuffle' g l gen) lengths groups
