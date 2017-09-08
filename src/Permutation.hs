module Permutation where

import Control.Monad.Random
import Data.Permute (Permute)
import qualified Data.Permute.MPermute as Permute

-- | Generate a random permutation of @n@ elements by Knuth shuffles.
shuffle :: Int -> IO Permute
shuffle n = do
  p <- Permute.newPermute n
  let
    end = n - 1
    shuffle1 i
      | i < end = do
          j <- getRandomR (i, end)
          Permute.swapElems p i j
          shuffle1 (i + 1)
      | otherwise = pure ()
  shuffle1 0
  Permute.unsafeFreeze p
