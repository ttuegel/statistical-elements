module Samples where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as V

import Combination (Choose)
import qualified Combination
import Linear
import Permutation (Permute)
import qualified Permutation

selectInputs :: M Double -> Choose -> M Double
selectInputs samples selection =
  samples ?? (All, Pos ixs)
  where
    selected = (+1) . fromIntegral <$> Combination.elems selection
    ixs = V.fromList (0 : selected)

permuteSamples :: M Double -> Permute -> M Double
permuteSamples samples permutation =
  samples ?? (Pos ixs, All)
  where
    ixs = V.fromList (fromIntegral <$> Permutation.elems permutation)

inputSubsets :: M Double -> Vector Choose
inputSubsets samples = do
  k <- Vector.enumFromN 1 (p - 1)
  Vector.unfoldr enumSubsets (Combination.choose p k)
  where
    p = cols samples - 1
    enumSubsets a = (\b -> (,) b b) <$> Combination.next a

shuffleSamples :: M Double -> IO Permute
shuffleSamples samples = Permutation.shuffle (rows samples)
