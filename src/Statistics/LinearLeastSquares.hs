{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Statistics.LinearLeastSquares where

import qualified Statistics.Sample as Sample
import qualified Data.Vector.Storable as V

import Linear


data LLS =
  LLS
  { coeffs :: V Double
  , scoreZs :: V Double
  }
  deriving (Show)

scoreZ :: Double  -- ^ variance estimator
       -> Double  -- ^ coefficient
       -> Double  -- ^ diagonal element
       -> Double  -- ^ z-score
scoreZ var beta v = beta / (var * sqrt v)

-- | Residual sum-of-squares
rss :: LLS -> M Double -> V Double -> Double
rss lls inp outp =
  let
    outp' = fromList (predict lls <$> toRows inp)
    res = outp - outp'
  in
    res <.> res

-- | Variance estimator based on 'residualSumOfSquares'.
variance :: LLS -> M Double -> V Double -> Double
variance lls inp outp =
  let
    n = fromIntegral (rows inp)
    p = fromIntegral (cols inp)
  in
    sqrt (rss lls inp outp / (n - p - 1))

fit :: M Double  -- ^ inputs, n samples (rows) and p variables (columns)
    -> V Double  -- ^ outputs, n samples
    -> LLS
fit inp outp =
  let
    -- lift inp into projective space
    x = matrix 1 (replicate (rows inp) 1) ||| inp
    inv_xTx = inv (tr x <> x)
    -- least squares fit coefficients
    coeffs = inv_xTx #> (tr x #> outp)
    scoreZs =
      let
        var = variance self inp outp
      in
        V.zipWith (scoreZ var) coeffs (takeDiag inv_xTx)
    self = LLS {..}
  in
    self

predict :: LLS  -- ^ fit coefficients
        -> V Double  -- ^ p-vector of inputs
        -> Double  -- ^ predicted output
predict (LLS {..}) inp =
  let
    -- lift inp into projective space
    x = V.cons 1 inp
  in
    coeffs <.> x

scoreF :: LLS  -- ^ fit with all variables
       -> M Double  -- ^ all inputs
       -> LLS  -- ^ fit with select variables
       -> M Double  -- ^ selected inputs
       -> V Double  -- ^ outputs
       -> Double
scoreF lls1 inp1 lls2 inp2 outp =
  let
    n = fromIntegral (rows inp1)
    p1 = fromIntegral (cols inp1)
    p2 = fromIntegral (cols inp2)
    rss1 = rss lls1 inp1 outp
    rss2 = rss lls2 inp2 outp
  in
    (rss2 - rss1) * (n - p2 - 1) / (rss2 * (p2 - p1))

scaleInputs :: M Double -> M Double
scaleInputs = fromColumns . map scaleColumn . toColumns
  where
    scaleColumn xs =
      let
        mean = Sample.mean xs
        stdDev = Sample.stdDev xs
      in
        cmap (\x -> (x - mean) / stdDev) xs
