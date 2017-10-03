module Statistics.Validation.Cross where

import Control.Lens.Fold
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Generic.Lens
import qualified Data.Vector.Storable as V
import Refined

import Linear
import Permutation
import Samples
import Sum

data Cross =
  Cross
  { validations :: Vector (M Double, M Double)
    -- ^ validation sets of (training, test) data
  }
  deriving Show

-- | Perform cross validation using the given validation sets and loss function.
crossValidate
  :: Vector (M Double, M Double)
  -> (Double -> Double -> Double)  -- ^ loss function
  -> (M Double -> a)  -- ^ train model
  -> (a -> M Double -> V Double)  -- ^ make predictions
  -> (Double, Double)  -- ^ standard error, cross validation error

crossValidate sets loss train predict =
  (stdErrOf traverseVs estimates, meanOf traverseVs estimates)
  where
    traverseVs :: Fold (Vector (V Double)) Double
    traverseVs = traverse . vectorTraverse

    estimates = estimatedLosses <$> sets
    estimatedLosses (training, testing) =
      let
        testOut = outputs testing
        testInp = inputs testing
        fitOut = predict (train training) testInp
      in
        V.zipWith loss testOut fitOut

-- | Select @n@ cross-validation sets from the given samples.
-- If a permutation is not supplied, one will be randomly generated.
validationSets
  :: M Double  -- ^ samples
  -> Permute  -- ^ permutation applied to input samples
  -> Refined (GreaterThan 1) Int -- ^ @n@, number of cross-validation sets
  -> Vector (M Double, M Double)

validationSets samples perm (unrefine -> n) =
  Vector.generate n sets
  where
    sets i =
      let
        (before, rest) = Vector.splitAt i blocks
        test = Vector.head rest
        after = Vector.tail rest
        train = Vector.foldl1' (===) (before Vector.++ after)
      in
        (train, test)

    shuffled = permuteSamples samples perm

    blocks = Vector.generate n block
      where
        (q, r) = quotRem (rows shuffled) n
        block i =
          let
            len
              | i < r = q + 1
              | otherwise = q
            start
              | i < r = (q + 1) * i
              | otherwise = r + q * i
            rowMap = asColumn (V.generate len (lookupRow start))
          in
            remap rowMap columnMap shuffled
        columnMap = asRow (V.enumFromN 0 (cols samples))
        lookupRow start i = fromIntegral (start + i)
