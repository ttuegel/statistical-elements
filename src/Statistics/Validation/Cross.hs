module Statistics.Validation.Cross where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as V
import Refined

import Linear
import Permutation

-- | Select @n@ cross-validation sets from the given samples.
validation :: M Double  -- ^ samples
           -> Permute
           -> Refined (GreaterThan 1) Int  -- ^ @n@, number of cross-validation sets
           -> Vector (M Double, M Double)  -- ^ (training, testing) sets
validation samples perm (unrefine -> n) =
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
    blocks = Vector.generate n block
      where
        (q, r) = quotRem (rows samples) n
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
            remap rowMap columnMap samples
        columnMap = asRow (V.enumFromN 0 (cols samples))
        lookupRow start i = fromIntegral (unsafeAt perm (start + i))
