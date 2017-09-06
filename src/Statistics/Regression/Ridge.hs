module Statistics.Regression.Ridge where

import qualified Statistics.Sample as Sample
import qualified Data.Vector.Storable as V

import Linear
import Statistics.Regression.Linear (standardize)

-- | The result of a ridge regression
data RR =
  RR
  { mean :: V Double
    -- ^ mean of training inputs (from standardization)
  , stdDev :: V Double
    -- ^ standard deviation of training inputs (from standardization)
  , coeffs :: V Double
    -- ^ fit coefficients
  , df :: Double
    -- ^ effective degrees of freedom
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
fit :: M Double  -- ^ inputs \(\mathbf{X}\),
                 -- n samples (rows) and p variables (columns)
    -> V Double  -- ^ outputs \(\mathbf{y}\), n samples
    -> Double  -- ^ ridge penalty \(\lambda\)
    -> RR
fit inp outp penalty =
  let
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
    -- effective degrees of freedom
    df = V.sum (singulars * penalized)
    self = RR {..}
  in
    self
