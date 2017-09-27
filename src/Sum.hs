module Sum where

import Control.Lens
import Numeric.Sum

import Loss

summationOf :: Summation s => Fold t Double -> s -> t -> s
summationOf f = foldlOf f add

meanOf :: Fold t Double -> t -> Double
meanOf f =
  let
    iter (partial, count) a = (add partial a, count + 1)
    start = (zero, 0 :: Int)
    getMean (partial, count) = kbn partial / fromIntegral count
  in getMean . foldlOf f iter start

biasedVarianceOf :: Fold t Double -> t -> Double
biasedVarianceOf f t =
  let
    mean = meanOf f t
    iter (partial, count) a =
      (add partial (squared (-) a mean), count + 1)
    start = (zero, 0 :: Int)
    getVariance (partial, count) = kbn partial / fromIntegral count
  in
    getVariance (foldlOf f iter start t)

varianceOf :: Fold t Double -> t -> Double
varianceOf f t =
  let
    mean = meanOf f t
    iter (partial, count) a =
      (add partial (squared (-) a mean), count + 1)
    start = (zero, 0 :: Int)
    getVariance (partial, count) = kbn partial / fromIntegral (count - 1)
  in
    getVariance (foldlOf f iter start t)

stdDevOf :: Fold t Double -> t -> Double
stdDevOf f = sqrt . varianceOf f

stdErrOf :: Fold t Double -> t -> Double
stdErrOf f t =
  let
    mean = meanOf f t
    iter (partial, count) a =
      (add partial (squared (-) a mean), count + 1)
    start = (zero, 0 :: Int)
    getStdErr (partial, count) =
      let n = fromIntegral count in
      sqrt (kbn partial / (n * n - n))
  in
    getStdErr (foldlOf f iter start t)
