module Statistics.Regression.BestSubset where

import Data.Choose
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as V
import Refined
import qualified Statistics.Sample as Sample

import Linear
import Loss
import Samples
import Statistics.Regression.LeastSquares (LeastSquares, leastSquares)
import qualified Statistics.Regression.LeastSquares as LeastSquares
import Statistics.Validation.Cross

data Subset =
  Subset
  { selector :: Choose
  , fit :: LeastSquares
  }
  deriving (Show)

-- | The linear regression fit of a subset of the inputs \(\mathbf{X}\)
-- to the outputs \(\mathbf{y}\)
-- minimizing the residual squared error,
-- \[
-- \mathsf{E}(\lambda)
-- = {(\mathbf{y} - \mathbf{X} \beta)}^{T}
--   {(\mathbf{y} - \mathbf{X} \beta)}
-- \]
-- given by 'rss'.
subset :: Choose  -- ^ subset
       -> M Double  -- ^ n samples (rows), one output and p inputs (columns)
       -> Subset
subset selector samples =
  let
    outp = outputs samples
    inp = inputs samples
    (fit, _) = leastSquares (asColumn outp ||| selectInputs inp selector)
  in
    Subset {..}

losses :: M Double  -- ^ samples
       -> Refined (GreaterThan 1) Int  -- ^ number of cross-validation sets
       -> Refined (GreaterThan 1) Int  -- ^ subset size
       -> IO (Vector (Choose, (Double, Double)))
losses samples crossSize subsetSize = do
  perm <- shuffleSamples samples
  (_, cr) <- validation samples (Just perm) crossSize
  pure $ do
    selector <- inputSubsets (inputs samples) subsetSize
    let errEstimate = Statistics.Validation.Cross.cross cr
                      (squared (-))
                      (subset selector)
                      predicts
    pure (selector, errEstimate)

validateSubset :: M Double -> Refined (GreaterThan 1) Int -> Choose -> IO (Double, Double)
validateSubset samples crossSize selector = do
  (_, cr) <- validation samples Nothing crossSize
  pure $ Statistics.Validation.Cross.cross cr (squared (-)) (subset selector) predicts

predicts :: Subset -> M Double -> V Double
predicts (Subset {..}) inp =
  LeastSquares.predicts fit (selectInputs inp selector)

bestSubset :: M Double  -- ^ samples
           -> Refined (GreaterThan 1) Int  -- ^ number of cross-validation sets
           -> Refined (GreaterThan 1) Int  -- ^ subset size
           -> IO (Subset, Double, Double)
bestSubset samples crossSize subsetSize = do
  (selector, (err, var)) <- Vector.minimumBy (comparing (fst . snd))
                            <$> losses samples crossSize subsetSize
  pure (subset selector samples, err, var)

residuals :: Subset -> M Double -> V Double
residuals (Subset {..}) samples =
  let
    outp_actual = outputs samples
    inp = selectInputs (inputs samples) selector
    outp_predicted = LeastSquares.predicts fit inp
  in
    outp_predicted - outp_actual

squaredResiduals :: Subset -> M Double -> V Double
squaredResiduals fit samples =
  let res = residuals fit samples in res * res

sumOfSquaredResiduals :: Subset -> M Double -> Double
sumOfSquaredResiduals fit samples = V.sum (squaredResiduals fit samples)

meanSquaredError :: Subset -> M Double -> Double
meanSquaredError fit samples = Sample.mean (squaredResiduals fit samples)
