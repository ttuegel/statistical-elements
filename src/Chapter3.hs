module Chapter3 where

import Control.Lens
import qualified Data.Vector as Vector
import Data.Vector.Generic.Lens
import qualified Data.Vector.Storable as V
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.State
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
    cvSets = validationSets training perm crossSize
    (cvStdErr, cvErr) =
      crossValidate cvSets (squared (-))
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
  (training, _) <- parseFile "./data/prostate/prostate.data"
  perm <- shuffleSamples training
  let
    Right crossSize = refine 10
    pointsWithErr = do
      Right subsetSize <- refine <$> Vector.enumFromN 1 (cols (inputs training))
      ((cvStdErr, cvErr), _) <- validateSubsets training perm crossSize subsetSize
      let n :: Double = fromIntegral (unrefine subsetSize)
      pure (symErrPoint n cvErr 0 cvStdErr)
    points = do
      pt <- pointsWithErr
      pure ((ev_best . ep_x) pt, (ev_best . ep_y) pt)
    p = toRenderable $ do
      plot $ liftEC $ do
        c <- takeColor
        plot_points_values .= Vector.toList points
        plot_points_style . point_color .= c
        plot_points_style . point_radius .= 2
      plot $ liftEC $ plot_errbars_values .= Vector.toList pointsWithErr
  renderableToWindow p 800 600
