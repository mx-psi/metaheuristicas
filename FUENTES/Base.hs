{-|
Module      : Base
Description : Basic types
-}
module Base(
  Class
  ,Example
  ,Examples
  ,Feature
  ,Weights
  ,Point
  ,DataSet
  ,Algorithm
  ,Population
  ,Solution(Sol)
  ,getV
  ,getN
  ,getEval
  ,getData
  ,getSize
  ,getClass
  ,getFeats
  ,nFeats
  ) where

import qualified Data.Vector.Unboxed  as U(Vector, length)
import qualified Data.Set as S

    ----------------
    -- BASE TYPES --
    ----------------

-- | The type of classes
type Class = Int

-- | The type of features
type Feature = Double

-- | The type of vectors of weights
type Weights = U.Vector Double

-- | The type of points from a dataset
type Point = U.Vector Feature

-- | The type of examples: a pair of a point and its associated class
type Example = (Point, Class)

-- | The type of a dataset of classified points
type Examples = [Example]
type DataSet = (Int, Examples)

-- | The type of an APC algorithm
type Algorithm = DataSet -> Weights

-- | The type of a Solution
data Solution = Sol {
  getEval :: !Double, -- | The goodness of the Solution
  getN :: !Int,       -- | The Solution number
  getV :: Weights     -- | The Solution
  }

instance Eq Solution where
  _ == _ = False -- Nunca hay dos soluciones iguales

instance Ord Solution where
  (Sol x _ _ ) <= (Sol y _ _) = x <= y
  (Sol x _ _) > (Sol y _ _) = x > y
  compare x y
    | x <= y    =  LT
    | otherwise =  GT
  max new current = if getEval new > getEval current then new else current


type Population = S.Set Solution


--------------------
-- BASE FUNCTIONS --
--------------------

-- | Get the Point from an Example
getFeats :: Example -> Point
getFeats = fst

nFeats :: DataSet -> Int
nFeats = U.length . getFeats . head . snd

getData :: DataSet -> Examples
getData = snd

getSize :: DataSet -> Int
getSize = fst

-- | Get the Class of a Point
getClass :: Example -> Class
getClass = snd
