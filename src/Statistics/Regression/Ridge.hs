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

-- | Fit the provided training data.
fit :: M Double  -- ^ inputs, n samples (rows) and p variables (columns)
    -> V Double  -- ^ outputs, n samples
    -> Double  -- ^ ridge penalty
    -> RR
fit inp outp lambda =
  let
    (stdInp, mean, stdDev) = standardize inp
    -- lift inp into projective space
    x = stdInp
    y0 = Sample.mean outp
    y = V.map (\yi -> yi - y0) outp
    (u, d, v) = thinSVD x
    dd_plus_lambda_inv = diag (cmap (\di -> recip $ di * di + lambda) d)
    -- least squares fit coefficients
    coeffs = V.cons y0 (v #> dd_plus_lambda_inv #> diag d #> tr u #> y)
    df = V.sum (cmap (\d_i -> d_i * d_i / (d_i * d_i + lambda)) d)
    self = RR {..}
  in
    self
