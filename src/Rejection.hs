module Rejection where

import Exception

import Control.Monad.Random.Strict

-- | Sample the desired distribution by rejection using the proposal
-- distribution. Note that the acceptance probability must be chosen so that
-- @f(x) <= M g(x)@ for all @x@.
sample :: (Monad m, RandomGen g) =>
          RandT g m a  -- ^ sample from the proposal distribution, @x@
       -> (a -> Double)  -- ^ proposal distribution, @g(x)@
       -> Double  -- ^ acceptance probability, @1/M@
       -> (a -> Double)  -- ^ desired distribution, @f(x)@
       -> RandT g m a  -- ^ sample from the desired distribution
sample propose proposed accept desired = sample_go
  where
    sample_go = do
      y <- propose
      u <- getRandomR (0, 1)
      let
        f = desired y
        g = proposed y
        prob = accept * f * g
      when (prob > 1) (throw ProbOverflow)
      if u <= prob
        then pure y
        else sample_go

