module Statistics.Regression.BestSubset where

import Data.Choose
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Refined

import Linear
import Samples
import Statistics.Regression.LeastSquares (LeastSquares, leastSquares)
import qualified Statistics.Regression.LeastSquares as LeastSquares
import Statistics.Validation.Cross

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
       -> LeastSquares
subset selector samples = (fst . leastSquares) (selectInputs samples selector)

losses :: M Double  -- ^ samples
       -> Refined (GreaterThan 1) Int  -- ^ number of cross-validation sets
       -> Refined (GreaterThan 1) Int  -- ^ subset size
       -> IO (Vector (Choose, Double))
losses samples crossSize subsetSize = do
  permutation <- shuffleSamples samples
  let validations = validation samples permutation crossSize
  pure $ do
    selector <- inputSubsets samples subsetSize
    let errEstimate = Statistics.Validation.Cross.cross
                      validations
                      LeastSquares.squaredLoss
                      (flip selectInputs selector)
                      (fst . LeastSquares.leastSquares)
                      LeastSquares.predicts
    pure (selector, errEstimate)

predicts :: Choose -> LeastSquares -> M Double -> V Double
predicts selector fit samples =
  LeastSquares.predicts fit (selectInputs samples selector)

bestSubset :: M Double  -- ^ samples
           -> Refined (GreaterThan 1) Int  -- ^ number of cross-validation sets
           -> Refined (GreaterThan 1) Int  -- ^ subset size
           -> IO (Choose, LeastSquares)
bestSubset samples crossSize subsetSize = do
  (selector, _) <- Vector.minimumBy (comparing snd)
                   <$> losses samples crossSize subsetSize
  pure (selector, subset selector samples)
