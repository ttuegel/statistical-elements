module Statistics.Regression.LeastSquares where

import qualified Data.Vector.Storable as V
import qualified Statistics.Sample as Sample

import Combination (Choose)
import qualified Combination
import Linear
import Samples

-- | The result of a linear least-squares regression.
data LeastSquares =
  LeastSquares
  { mean :: V Double
    -- ^ mean of training inputs (from standardization)
  , stdDev :: V Double
    -- ^ standard deviation of training inputs (from standardization)
  , coeffs :: V Double
    -- ^ fit coefficients
  }
  deriving (Show)

scoreZ :: Double  -- ^ variance estimator
       -> Double  -- ^ coefficient
       -> Double  -- ^ diagonal element
       -> Double  -- ^ z-score
scoreZ var beta v = beta / (var * sqrt v)

squaredLoss :: V Double -> V Double -> Double
squaredLoss x y = let r = x - y in (r <.> r)

meanSquaredLoss :: V Double -> V Double -> Double
meanSquaredLoss x y = let r = x - y in Sample.mean (r * r)

residuals :: LeastSquares -> M Double -> V Double
residuals lls samples =
  let
    outp_actual = outputs samples
    inp = inputs samples
    outp_predicted = fromList (predict lls <$> toRows inp)
  in
    outp_predicted - outp_actual

squaredResiduals :: LeastSquares -> M Double -> V Double
squaredResiduals fit samples =
  let res = residuals fit samples in res * res

sumOfSquaredResiduals :: LeastSquares -> M Double -> Double
sumOfSquaredResiduals fit samples = V.sum (squaredResiduals fit samples)

varianceEstimate :: LeastSquares -> M Double -> Double
varianceEstimate fit samples =
  let
    n = rows samples
    p = cols (inputs samples)
  in
    sumOfSquaredResiduals fit samples / fromIntegral (n - p - 1)

meanSquaredError :: LeastSquares -> M Double -> Double
meanSquaredError fit samples = Sample.mean (squaredResiduals fit samples)

-- | The linear regression fit of the inputs \(\mathbf{X}\)
-- to the outputs \(\mathbf{y}\).
-- Linear regression minimizes the residual squared error,
-- \[
-- \mathsf{E}(\lambda)
-- = {(\mathbf{y} - \mathbf{X} \beta)}^{T}
--   {(\mathbf{y} - \mathbf{X} \beta)}
-- \]
-- given by 'rss'.
leastSquares :: M Double  -- ^ n samples (rows), one output and p inputs (columns)
             -> (LeastSquares, V Double)
leastSquares samples =
  let
    outp = flatten (samples ?? (All, Take 1))
    inp = samples ?? (All, Drop 1)
    (stdInp, mean, stdDev) = standardize inp
    -- standardized input values lifted into projective space
    projInp = matrix 1 (replicate (rows stdInp) 1) ||| stdInp
    (colSpace, rightTri) = thinQR projInp
    -- least squares fit coefficients
    coeffs = inv rightTri #> (tr colSpace #> outp)
    self = LeastSquares {..}
    scoreZs =
      let
        var = sqrt (varianceEstimate self samples)
        covarDenorm = tr rightTri <> rightTri
      in
        V.zipWith (scoreZ var) coeffs (takeDiag (inv covarDenorm))
  in
    (self, scoreZs)

predict :: LeastSquares  -- ^ fit coefficients
        -> V Double  -- ^ p-vector of inputs
        -> Double  -- ^ predicted output
predict (LeastSquares {..}) inp =
  V.head coeffs + V.tail coeffs <.> standardizeWith (mean, stdDev) inp

predicts :: LeastSquares  -- ^ fit coefficients
         -> M Double  -- ^ p columns of inputs
         -> V Double  -- ^ predicted output
predicts fit inp = V.fromList (predict fit <$> toRows inp)

scoreF :: M Double  -- ^ all inputs
       -> Choose
       -> LeastSquares  -- ^ fit with all variables
       -> LeastSquares  -- ^ fit with select variables
       -> Double
scoreF samples selected lls1 lls2 =
  let
    n = fromIntegral (rows samples)
    p1 = fromIntegral (cols samples - 1)
    p2 = fromIntegral (Combination.size selected)
    rss1 = sumOfSquaredResiduals lls1 samples
    rss2 = sumOfSquaredResiduals lls2 (selectInputs samples selected)
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
