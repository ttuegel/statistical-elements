module Permutation where

import Control.Monad.Random
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as V

import Linear

-- | Generate a random permutation of @n@ elements.
permutation :: (Num i, Storable i) => Int  -- ^ @n@
            -> IO (V i)
permutation len = do
  p <- V.unsafeThaw (V.enumFromN 0 len)
  let
    end = len - 1
    permutation1 i
      | i < len = do
          j <- getRandomR (i, end)
          V.swap p i j
          permutation1 (i + 1)
      | otherwise = pure ()
  permutation1 0
  V.unsafeFreeze p
