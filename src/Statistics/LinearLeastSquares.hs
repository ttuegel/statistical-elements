{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Statistics.LinearLeastSquares where

import Linear


newtype LeastSquares p a = LeastSquares (V (p + 1) a)

fit :: forall a (n :: Nat) (p :: Nat).
       (KnownNat n, KnownNat p, Semiring a) =>
       M p n a  -- ^ inputs: n samples of a p-vector
    -> V n a  -- ^ outputs
    -> LeastSquares p a
fit inp outp =
  let
    -- lift inp into projective space
    x = projectiveM inp
    -- least squares fit coefficients
    beta = inv (x ## tr x) #> (x #> outp)
  in
    LeastSquares beta

predict :: LeastSquares p a  -- ^ fit coefficients
        -> V p a  -- ^ p-vector of inputs
        -> a  -- ^ predicted output
predict (LeastSquares beta) inp =
  let
    -- lift inp into projective space
    x = projectiveV inp
  in
    beta <.> x
