module Samples where

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
    ixs = V.fromList ((+1) . fromIntegral <$> Combination.elems selection)

permuteSamples :: M Double -> Permute -> M Double
permuteSamples samples permutation =
  samples ?? (Pos ixs, All)
  where
    ixs = V.fromList (fromIntegral <$> Permutation.elems permutation)
