module Lattice where

import Control.Applicative (Applicative(..))
import Control.Monad (filterM)
import Data.Ord (Ord(..))
import Data.List (iterate, take)
import System.IO (IO)
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
     -> IO [V n Double]  -- ^ occupied points
fill dist points = do
  let
    check pt = do
      r <- randomRIO (0, 1)
      pure (r <= dist pt)
  filterM check points
