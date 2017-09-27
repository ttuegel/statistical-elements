module Chapter3 where

import Data.Vector.Generic.Lens
import qualified Data.Vector.Storable as V
import Refined

import Data.Prostate (parseFile)
import Loss
import Samples
import Statistics.Regression.BestSubset (Subset)
import qualified Statistics.Regression.BestSubset as BestSubset
import Statistics.Regression.LeastSquares (LeastSquares)
import qualified Statistics.Regression.LeastSquares as LeastSquares
import qualified Statistics.Validation.Cross as Cross
import Sum

data Result r =
  Result
  { trained :: r
  , testErr :: Double
  , testStdErr :: Double
  , cvErr :: Double
  , cvStdErr :: Double
  }
  deriving Show

leastSquares :: IO (Result LeastSquares)
leastSquares = do
  (training, testing) <- parseFile "./data/prostate/prostate.data"
  let
    (trained, _) = LeastSquares.leastSquares training
    predicted = LeastSquares.predicts trained (inputs testing)
    actual = outputs testing
    losses = V.zipWith (squared (-)) predicted actual
    testErr = meanOf vectorTraverse losses
    testStdErr = stdErrOf vectorTraverse losses

  let Right crossSize = refine 10
  (_, vs) <- Cross.validation training Nothing crossSize
  let
    (cvErr, cvStdErr) =
      Cross.cross vs (squared (-))
      (fst . LeastSquares.leastSquares) LeastSquares.predicts
  pure Result {..}

bestSubset :: IO (Result Subset)
bestSubset = do
  (training, testing) <- parseFile "./data/prostate/prostate.data"
  let
    Right crossSize = refine 10
    Right subsetSize = refine 2
  (trained, cvErr, cvStdErr) <- BestSubset.bestSubset training crossSize subsetSize
  let
    predicted = BestSubset.predicts trained (inputs testing)
    actual = outputs testing
    losses = V.zipWith (squared (-)) predicted actual
    testErr = meanOf vectorTraverse losses
    testStdErr = stdErrOf vectorTraverse losses
  pure Result {..}
