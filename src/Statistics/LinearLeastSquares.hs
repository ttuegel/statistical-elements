{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Statistics.LinearLeastSquares where

import Prelude.Local

import Algebra
import Linear


data LeastSquares p =
  LeastSquares
  { coeffs :: V (p + 1) Double
  , scoreZs :: V (p + 1) Double
  }

scoreZ :: Double  -- ^ variance estimator
       -> Double  -- ^ coefficient
       -> Double  -- ^ diagonal element
       -> Double  -- ^ z-score
scoreZ var beta v = beta / (var * sqrt v)

residualSumOfSquares :: (KnownNat n, 1 <= n) =>
                        M (p + 1) n Double -> V (p + 1) Double -> V n Double -> Double
residualSumOfSquares inp beta outp =
  let
    outp' = (fromList . map Xd) $ (<.>) beta <$> toColumns inp
    res = outp - outp'
  in
    res <.> res

-- | Variance estimator based on 'residualSumOfSquares'.
variance :: forall (n :: Nat) (p :: Nat).
            (KnownNat n, KnownNat p, 1 <= n) =>
            M (p + 1) n Double -> V (p + 1) Double -> V n Double -> Double
variance x coeffs outp =
  let
    n = (fromIntegral . fromEnum) (natVal (Proxy :: Proxy n))
    p = (fromIntegral . fromEnum) (natVal (Proxy :: Proxy p))
  in
    residualSumOfSquares x coeffs outp / (n - p - 1)

fit :: forall (n :: Nat) (p :: Nat).
       (KnownNat n, KnownNat p, 1 <= n) =>
       M p n Double  -- ^ inputs: n samples of a p-vector
    -> V n Double  -- ^ outputs
    -> LeastSquares p
fit inp outp =
  let
    -- lift inp into projective space
    x :: M (p + 1) n Double
    x = projectiveM inp
    inv_xxT :: M (p + 1) (p + 1) Double
    inv_xxT = inv (x ## tr x)
    -- least squares fit coefficients
    coeffs :: V (p + 1) Double
    coeffs = inv_xxT #> (x #> outp)
    scoreZs =
      let
        var = variance x coeffs outp
      in
        zipWithV (scoreZ var) coeffs (takeDiag inv_xxT)
  in
    LeastSquares {..}

predict :: LeastSquares p  -- ^ fit coefficients
        -> V p Double  -- ^ p-vector of inputs
        -> Double  -- ^ predicted output
predict (LeastSquares {..}) inp =
  let
    -- lift inp into projective space
    x = projectiveV inp
  in
    coeffs <.> x
