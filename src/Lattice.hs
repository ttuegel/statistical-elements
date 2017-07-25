module Lattice where

import Prelude.Local

import System.Random (randomRIO)

import Algebra
import Linear

lattice :: KnownNat n => [(Int, V n Double)] -> [V n Double]
lattice [] = []
lattice [(n, a)] = take n (iterate (+ a) zero)
lattice ((n, a) : as) = do
  x <- take n (iterate (+ a) zero)
  y <- lattice as
  pure (x + y)

fill :: (V n Double -> Double)  -- ^ distribution
     -> [V n Double]  -- ^ lattice
     -> IO [(V n Double, Bool)]  -- ^ occupied points
fill dist points = do
  let
    check pt = do
      r <- randomRIO (0, 1)
      pure (r <= dist pt)
  mapM (\p -> (,) p <$> check p) points
