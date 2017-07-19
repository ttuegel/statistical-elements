module Statistics.LeastSquares where

import Numeric.LinearAlgebra

newtype LeastSquares a = LeastSquares (Matrix a)

fit :: (Field a) =>
       Matrix a  -- ^ inputs: n samples of a p-vector
    -> Matrix a  -- ^ outputs: n samples of a k-vector
    -> LeastSquares a
fit inp outp =
  let
    -- lift inp into projective space:
    -- augment the matrix with a column of 1s
    x = konst 1 (rows inp, 1) ||| inp
    -- least squares fit coefficients
    beta = inv (tr x <> x) <> tr x <> outp
  in
    LeastSquares beta

predict :: (Field a) =>
           LeastSquares a  -- ^ p-by-k fit coefficients
        -> Vector a  -- ^ p-vector of inputs
        -> Vector a  -- ^ k-vector of predicted outputs
predict (LeastSquares beta) inp =
  let
    -- lift inp into projective space
    x = vjoin [konst 1 1, inp]
  in
    tr beta #> x
