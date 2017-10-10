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
import Permutation
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

validateSubsets
  :: M Double  -- ^ samples
  -> Permute
  -> Refined (GreaterThan 1) Int  -- ^ number of cross validation sets
  -> Refined (GreaterThan 0) Int  -- ^ subset size
  -> Vector ((Double, Double), Choose)
  -- ^ (selected inputs, (cross validation error, standard error))
validateSubsets samples perm crossSize subsetSize = do
  let sets = validationSets samples perm crossSize
  selector <- inputSubsets (inputs samples) subsetSize
  pure (validateSubset sets selector, selector)

validateSubset
  :: Vector (M Double, M Double)  -- ^ cross validation sets
  -> Choose                       -- ^ selected subset of inputs
  -> (Double, Double)             -- ^ (standard error, cross validation error)
validateSubset sets selector =
  crossValidate sets (squared (-)) (subset selector) predicts

predicts :: Subset -> M Double -> V Double
predicts (Subset {..}) inp =
  LeastSquares.predicts fit (selectInputs inp selector)

bestSubset
  :: M Double  -- ^ samples
  -> Permute
  -> Refined (GreaterThan 1) Int  -- ^ number of cross-validation sets
  -> Refined (GreaterThan 0) Int  -- ^ subset size
  -> ((Double, Double), Subset)
bestSubset samples perm crossSize subsetSize =
  (err, subset selector samples)
  where
    (err, selector) = Vector.minimumBy (comparing (snd . fst))
                      (validateSubsets samples perm crossSize subsetSize)

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
