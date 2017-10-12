module Statistics.Regression.Ridge where

import Data.Vector (Vector)
import qualified Data.Vector.Storable as V
import qualified Statistics.Sample as Sample

import Linear
import Loss
import Statistics.Regression.LeastSquares (standardize, standardizeWith)
import Statistics.Validation.Cross

-- | The result of a ridge regression
data Ridge =
  Ridge
  { mean :: V Double
    -- ^ mean of training inputs (from standardization)
  , stdDev :: V Double
    -- ^ standard deviation of training inputs (from standardization)
  , coeffs :: V Double
    -- ^ fit coefficients
  }
  deriving (Show)

-- | The ridge regression fit with the given penalty.
-- Ridge regression does not minimize the residual squared error,
-- rather it minimizes a modified error estimator
-- \[
-- \mathsf{E}(\lambda)
-- = {(\mathbf{y} - \mathbf{X} \beta)}^{T}
--   {(\mathbf{y} - \mathbf{X} \beta)}
-- + \lambda \beta^{T} \beta
-- \]
-- which effectively reduces model complexity
-- by shrinking the coefficients \(\beta\).
ridge :: Double  -- ^ ridge penalty \(\lambda\)
      -> M Double  -- ^ n samples (rows), one output and p variables (columns)
      -> Ridge
ridge penalty samples =
  let
    outp = flatten (samples ?? (All, Take 1))
    inp = samples ?? (All, Drop 1)
    (stdInp, mean, stdDev) = standardize inp
    meanOutp = Sample.mean outp
    -- centered outputs
    centerOutp = outp - scalar meanOutp
    (colSpace, singulars, rowSpace) = thinSVD stdInp
    -- penalized singular values of 'stdInp'
    penalized = singulars / (singulars * singulars + scalar penalty)
    -- least squares fit coefficients
    coeffs = V.cons meanOutp
             (rowSpace #> diag penalized #> tr colSpace #> centerOutp)
  in
    Ridge {..}

validateRidge
  :: Vector (M Double, M Double)  -- ^ cross validation sets
  -> Double                       -- ^ penalty
  -> (Double, Double)             -- ^ (standard error, cross validation error)
validateRidge sets penalty =
  crossValidate sets (squared (-)) (ridge penalty) predicts

predict :: Ridge  -- ^ fit coefficients
        -> V Double  -- ^ p-vector of inputs
        -> Double  -- ^ predicted output
predict (Ridge {..}) inp =
  V.head coeffs + V.tail coeffs <.> standardizeWith (mean, stdDev) inp

predicts :: Ridge  -- ^ fit coefficients
         -> M Double  -- ^ p columns of inputs
         -> V Double  -- ^ predicted output
predicts fit inp = V.fromList (predict fit <$> toRows inp)
