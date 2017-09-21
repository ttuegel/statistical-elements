module Statistics.Regression.BestSubset where

import Data.Choose
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as V

import Linear
import Statistics.Regression.Linear (LLS)
import qualified Statistics.Regression.Linear as LLS

-- | The linear regression fit of a subset of the inputs \(\mathbf{X}\)
-- to the outputs \(\mathbf{y}\)
-- minimizing the residual squared error,
-- \[
-- \mathsf{E}(\lambda)
-- = {(\mathbf{y} - \mathbf{X} \beta)}^{T}
--   {(\mathbf{y} - \mathbf{X} \beta)}
-- \]
-- given by 'rss'.
fit :: M Double  -- ^ n samples (rows), one output and p inputs (columns)
    -> Choose  -- ^ subset
    -> LLS
fit samples subset = LLS.fit selected
  where
    selected = samples ?? (All, Pos selector)
    selector = (V.fromList . (0 :)) (fromIntegral <$> elems subset)

subsets :: M Double -> Vector Choose
subsets samples = do
  k <- Vector.enumFromN 1 (p - 1)
  Vector.unfoldr (\a -> (\b -> (,) b b) <$> next a) (choose p k)
  where
    p = cols samples - 1

fits :: M Double -> Vector (Choose, LLS)
fits samples = do
  subset <- subsets samples
  pure (subset, fit samples subset)
