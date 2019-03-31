{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Read
Description : Module for reading datapoints and normalizing them
-}
module Read(readExamples) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Vector.Unboxed  as U
import Data.List
import Base

-- | Reads a Feature from a Text
-- Raises error if Text is not a Feature
readFeat :: T.Text -> Feature
readFeat x = case TR.rational x of
  Right (d, "") -> d
  Right (_, xs) -> error ("[readFeat] Didn't expect: " ++ show xs)
  Left  msg     -> error msg

-- | Reads a Class from a Text
-- Raises error if Text is not a Class
readClass :: T.Text -> Class
readClass x = case TR.decimal x of
  Right (d, "") -> d
  Right (_, xs) -> error ("[readClass] Didn't expect: " ++ show xs)
  Left  msg     -> error msg

-- | Gets an ARFF Example from Text
-- Last coordinate is asumed to be the class
readExample :: T.Text -> Example
readExample text = (U.fromList $ map readFeat (init xs), readClass (last xs))
  where xs = T.splitOn "," text

-- | Gets a list of Examples from ARFF formatted text
-- Currently assumes all coordinates are Doubles except the last
getExamples :: T.Text -> Examples
getExamples text = map readExample rawExamples
  where rawExamples = nub $ drop 1 $ dropWhile (/= "@data") (T.lines text)

-- | Normalizes a dataset according to the following formula
-- $x_j^N = \frac{x_j - \min_j}{\max_j - \min_j}$
normalize :: Examples -> Examples
normalize ps = map norm ps
 where
  maxv = foldr1 (U.zipWith max) $ map getFeats ps
  minv = foldr1 (U.zipWith min) $ map getFeats ps
  norm (x, c) = (U.zipWith3 (\h l e -> (e - l) / (h - l)) maxv minv x, c)

-- | Reads and normalizes Examples from an ARFF formatted file
readExamples :: FilePath -> IO Examples
readExamples = fmap (normalize . getExamples) . TIO.readFile
