module Statistics.Regression.Linear where

import qualified Statistics.Sample as Sample
import qualified Data.Vector.Storable as V

import Linear

-- | The result of a linear least-squares regression.
data LLS =
  LLS
  { mean :: V Double
    -- ^ mean of training inputs (from standardization)
  , stdDev :: V Double
    -- ^ standard deviation of training inputs (from standardization)
  , coeffs :: V Double
    -- ^ fit coefficients
  , scoreZs :: V Double
    -- ^ Z-score of fit coefficients
  }
  deriving (Show)

scoreZ :: Double  -- ^ variance estimator
       -> Double  -- ^ coefficient
       -> Double  -- ^ diagonal element
       -> Double  -- ^ z-score
scoreZ var beta v = beta / (var * sqrt v)

-- | Residual sum-of-squares
rss :: LLS -> M Double -> V Double -> Double
rss lls inp outp =
  let
    outp' = fromList (predict lls <$> toRows inp)
    res = outp - outp'
  in
    res <.> res

-- | Variance estimator based on 'rss'.
variance :: LLS -> M Double -> V Double -> Double
variance lls inp outp =
  let
    n = fromIntegral (rows inp)
    p = fromIntegral (cols inp)
  in
    sqrt (rss lls inp outp / (n - p - 1))

-- | The linear regression fit of the inputs \(\mathbf{X}\)
-- to the outputs \(\mathbf{y}\).
-- Linear regression minimizes the residual squared error,
-- \[
-- \mathsf{E}(\lambda)
-- = {(\mathbf{y} - \mathbf{X} \beta)}^{T}
--   {(\mathbf{y} - \mathbf{X} \beta)}
-- \]
-- given by 'rss'.
fit :: M Double  -- ^ n samples (rows), one output and p inputs (columns)
    -> LLS
fit samples =
  let
    outp = flatten (samples ?? (All, Take 1))
    inp = samples ?? (All, Drop 1)
    (stdInp, mean, stdDev) = standardize inp
    -- standardized input values lifted into projective space
    projInp = matrix 1 (replicate (rows stdInp) 1) ||| stdInp
    (colSpace, rightTri) = thinQR projInp
    -- least squares fit coefficients
    coeffs = inv rightTri #> (tr colSpace #> outp)
    scoreZs =
      let
        var = variance self inp outp
        covarDenorm = tr rightTri <> rightTri
      in
        V.zipWith (scoreZ var) coeffs (takeDiag (inv covarDenorm))
    self = LLS {..}
  in
    self

predict :: LLS  -- ^ fit coefficients
        -> V Double  -- ^ p-vector of inputs
        -> Double  -- ^ predicted output
predict (LLS {..}) inp =
  V.head coeffs + V.tail coeffs <.> standardizeWith (mean, stdDev) inp

scoreF :: LLS  -- ^ fit with all variables
       -> M Double  -- ^ all inputs
       -> LLS  -- ^ fit with select variables
       -> M Double  -- ^ selected inputs
       -> V Double  -- ^ outputs
       -> Double
scoreF lls1 inp1 lls2 inp2 outp =
  let
    n = fromIntegral (rows inp1)
    p1 = fromIntegral (cols inp1)
    p2 = fromIntegral (cols inp2)
    rss1 = rss lls1 inp1 outp
    rss2 = rss lls2 inp2 outp
  in
    (rss2 - rss1) * (n - p2 - 1) / (rss2 * (p2 - p1))

standardize :: M Double -> (M Double, V Double, V Double)
standardize inp =
  ((inp - asRow mean) / asRow stdDev, mean, stdDev)
  where
    column i = flatten ((¿) inp [i])
    mean = V.generate (cols inp) (\i -> Sample.mean (column i))
    stdDev = V.generate (cols inp) (\i -> Sample.stdDev (column i))

standardizeWith :: (V Double, V Double) -> V Double -> V Double
standardizeWith (mean, stdDev) inp = (inp - mean) / stdDev
