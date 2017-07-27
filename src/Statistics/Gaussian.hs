module Statistics.Gaussian where

import Linear

-- | Multivariate normal distribution.
-- The covariance matrix should be positive-definite;
-- if it is not, this function will return 'NaN'.
gaussian :: V Double  -- ^ mean
         -> H Double  -- ^ covariance
         -> V Double  -- ^ position
         -> Double  -- ^ probability density
gaussian mean (unSym -> cov) x =
  let
    twopi = 2 * pi :: Double
    norm = (recip . sqrt . det) (scale twopi cov)
    dev = x - mean
  in
    norm * exp (negate 0.5 * (dev <.> (inv cov #> dev)))
