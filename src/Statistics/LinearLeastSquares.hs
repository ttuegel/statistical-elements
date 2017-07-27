{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Statistics.LinearLeastSquares where

import Prelude.Local

import Algebra
import Linear


data LinearLeastSquares p =
  LinearLeastSquares
  { coeffs :: V (p + 1) Double
  , scoreZs :: V (p + 1) Double
  }

scoreZ :: Double  -- ^ variance estimator
       -> Double  -- ^ coefficient
       -> Double  -- ^ diagonal element
       -> Double  -- ^ z-score
scoreZ var beta v = beta / (var * sqrt v)

-- | Residual sum-of-squares
rss :: (KnownNat n, 1 <= n) =>
       LinearLeastSquares p -> M p n Double -> V n Double -> Double
rss lls inp outp =
  let
    outp' = (fromList . map Xd) $ predict lls <$> toColumns inp
    res = outp - outp'
  in
    res <.> res

-- | Variance estimator based on 'residualSumOfSquares'.
variance :: forall (n :: Nat) (p :: Nat).
            (KnownNat n, KnownNat p, 1 <= n) =>
            LinearLeastSquares p -> M p n Double -> V n Double -> Double
variance lls inp outp =
  let
    n = (fromIntegral . fromEnum) (natVal (Proxy :: Proxy n))
    p = (fromIntegral . fromEnum) (natVal (Proxy :: Proxy p))
  in
    sqrt (rss lls inp outp / (n - p - 1))

fit :: forall (n :: Nat) (p :: Nat).
       (KnownNat n, KnownNat p, 1 <= n) =>
       M p n Double  -- ^ inputs: n samples of a p-vector
    -> V n Double  -- ^ outputs
    -> LinearLeastSquares p
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
        var = variance self inp outp
      in
        zipWithV (scoreZ var) coeffs (takeDiag inv_xxT)
    self = LinearLeastSquares {..}
  in
    self

predict :: LinearLeastSquares p  -- ^ fit coefficients
        -> V p Double  -- ^ p-vector of inputs
        -> Double  -- ^ predicted output
predict (LinearLeastSquares {..}) inp =
  let
    -- lift inp into projective space
    x = projectiveV inp
  in
    coeffs <.> x

scoreF :: forall (p1 :: Nat) (p2 :: Nat) (n :: Nat).
          (KnownNat p1, KnownNat p2, KnownNat n, p2 <= p1, 1 <= n) =>
          LinearLeastSquares p1  -- ^ fit
       -> M p1 n Double  -- ^ all inputs
       -> LinearLeastSquares p2  -- ^ fit
       -> M p2 n Double  -- ^ selected inputs
       -> V n Double  -- ^ outputs
       -> Double
scoreF lls1 inp1 lls2 inp2 outp =
  let
    n = (fromIntegral . fromEnum) (natVal (Proxy :: Proxy n))
    p1 = (fromIntegral . fromEnum) (natVal (Proxy :: Proxy p1))
    p2 = (fromIntegral . fromEnum) (natVal (Proxy :: Proxy p2))
    rss1 = rss lls1 inp1 outp
    rss2 = rss lls2 inp2 outp
  in
    (rss2 - rss1) * (n - p2 - 1) / (rss2 * (p2 - p1))
