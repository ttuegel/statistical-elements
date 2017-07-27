{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Statistics.LinearLeastSquares where

import Linear


newtype LeastSquares p k a = LeastSquares (M (p + 1) k a)

fit :: forall a (k :: Nat) (n :: Nat) (p :: Nat).
       (KnownNat k, KnownNat n, KnownNat p, Semiring a) =>
       M p n a  -- ^ inputs: n samples of a p-vector
    -> M k n a  -- ^ outputs: n samples of a k-vector
    -> LeastSquares p k a
fit inp outp =
  let
    -- lift inp into projective space
    x = projectiveM inp
    -- least squares fit coefficients
    beta = inv (x ## tr x) ## x ## tr outp
  in
    LeastSquares beta

predict :: LeastSquares p k a  -- ^ p-by-k fit coefficients
        -> V p a  -- ^ p-vector of inputs
        -> V k a  -- ^ k-vector of predicted outputs
predict (LeastSquares beta) inp =
  let
    -- lift inp into projective space
    x = projectiveV inp
  in
    tr beta #> x
