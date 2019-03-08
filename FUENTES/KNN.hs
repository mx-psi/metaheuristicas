{-# LANGUAGE BangPatterns #-}

{-|
Module      : KNN
Description : Implements 1-NN
-}
module KNN(eval, rawEval, distW) where

import qualified Data.Vector.Unboxed  as U
import Base
import Data.List(minimumBy)
import Data.Function(on)
import Data.List.Zipper

----------------------------
-- CLASIFICACIÓN 1-NN CON --
--    VECTOR DE PESOS     --
----------------------------

-- | @clasifyW ws training p@ classifies 'p' using 1-NN with 'training' as training
-- data and 'ws' as weights for the distance
classW :: Weights -> Examples -> Point -> Class
classW !ws !ds !p = getClass neighbor
  where neighbor = minimumBy (compare `on` (distW ws p . getFeats)) ds

-- | Calculates weighted Euclidean distance between two vectors
distW :: Weights -> Point -> Point -> Double
distW !ws !xs !ys = U.sum $ U.zipWith3 (\w x y -> w*(x-y)^(2::Int)) ws xs ys

-- | Discards < 0.2 coordinates
norm :: Weights -> Weights
norm = U.map (\x -> if x < 0.2 then 0 else x)


-------------------------------
-- EVALUACIÓN EN PARTICIONES --
-------------------------------

-- | Función de evaluación completa
eval :: Weights -> Examples -> Examples -> (Double,Double)
eval ws training test = (100*precCross (norm ws) training test, 100*tasaRed ws)

-- | Calculates number of correctly classified instances
-- It uses
precCross :: Weights -> Examples -> Examples -> Double
precCross ws training test = foldr infer 0 test//length test
  where infer a !acc =  acc + fromEnum (classW ws training (getFeats a) == getClass a)


--------------------------------
-- EVALUACIÓN PARA ALGORITMOS --
--      (MÁS EFICIENTE)       --
--------------------------------

(//) :: Int -> Int -> Double
(//) = (/) `on` fromIntegral

-- | Función de evaluación raw (haría falta multiplicar por 50 para tener el valor real)
rawEval :: Weights -> DataSet -> Double
rawEval ws ds = tasaClas (norm ws) ds + tasaRed ws

-- | Mide el número de instancias clasificadas correctamente (precisión)
tasaClas :: Weights -> DataSet -> Double
tasaClas ws ds = leave1Out ws (getData ds)//(getSize ds)

-- | Calculates number of correctly classified instances
leave1Out :: Weights -> Examples -> Int
leave1Out ws ds = foldrz infer 0 (fromList ds)
  where infer z !acc = let a = cursor z in
          acc + fromEnum (classW ws (rest z) (getFeats a) == getClass a)
        rest z = let (Zip as bs) = delete z in as ++ bs

-- | Porcentaje de características descartadas (simplicidad)
tasaRed :: Weights -> Double
tasaRed ws = m//n
  where m = U.foldr' (\w acc -> acc + fromEnum (w < 0.2)) 0 ws
        n = U.length ws
