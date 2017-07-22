module Statistics.Gaussian where

import Linear

-- | Multivariate normal distribution.
-- The covariance matrix should be positive-definite;
-- if it is not, this function will return 'NaN'.
gaussian :: KnownNat n =>
            V n Double  -- ^ mean
         -> H n Double  -- ^ covariance
         -> V n Double  -- ^ position
         -> Double  -- ^ probability density
gaussian mean (unsym -> cov) x =
  let
    twopi = 2 * pi :: Double
    norm = (recip . sqrt . det) (twopi .* cov)
    dev = x - mean
  in
    norm * exp (negate 0.5 * (dev <.> (inv cov #> dev)))
