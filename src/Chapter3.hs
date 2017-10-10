module Chapter3 where

import Debug.Trace

import qualified Data.Vector as Vector
import Data.Vector.Generic.Lens
import qualified Data.Vector.Storable as V
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Gtk (renderableToWindow)
import Refined

import Data.Prostate (parseFile)
import Linear
import Loss
import Samples
import Statistics.Regression.BestSubset as BestSubset
import Statistics.Regression.LeastSquares (LeastSquares)
import qualified Statistics.Regression.LeastSquares as LeastSquares
import Statistics.Validation.Cross
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

  perm <- shuffleSamples training
  let
    Right crossSize = refine 10
    sets = validationSets training perm crossSize
    (cvStdErr, cvErr) =
      crossValidate sets (squared (-))
      (fst . LeastSquares.leastSquares) LeastSquares.predicts
  pure Result {..}

bestSubset :: IO (Result Subset)
bestSubset = do
  (training, testing) <- parseFile "./data/prostate/prostate.data"
  perm <- shuffleSamples training
  let
    Right crossSize = refine 10
    Right subsetSize = refine 2
    ((cvErr, cvStdErr), trained) =
      BestSubset.bestSubset training perm crossSize subsetSize
  let
    predicted = BestSubset.predicts trained (inputs testing)
    actual = outputs testing
    losses = V.zipWith (squared (-)) predicted actual
    testErr = meanOf vectorTraverse losses
    testStdErr = stdErrOf vectorTraverse losses
  pure Result {..}

plotSubsetsCrossValidation :: IO ()
plotSubsetsCrossValidation = do
  (training, testing) <- parseFile "./data/prostate/prostate.data"
  perm <- shuffleSamples training
  let
    Right crossSize = refine 10
    validations = do
      Right subsetSize <- refine . (\x -> traceShow x x) <$> Vector.enumFromN 1 (cols (inputs training))
      ((cvErr, cvStdErr), _) <- validateSubsets training perm crossSize subsetSize
      let n :: Double = fromIntegral (unrefine subsetSize)
      pure (n, cvErr, cvErr - cvStdErr, cvErr + cvStdErr)
    (Vector.toList -> x, Vector.toList -> y, _, _) = Vector.unzip4 validations
    p = toRenderable $ do
      plot (points "" (zip x y))
  renderableToWindow p 800 600
