module Linear
    ( module Numeric.LinearAlgebra
    , V, M, H
    ) where

import Numeric.LinearAlgebra hiding (Herm, Matrix, R, Vector)
import qualified Numeric.LinearAlgebra

type V = Numeric.LinearAlgebra.Vector
type M = Numeric.LinearAlgebra.Matrix
type H = Numeric.LinearAlgebra.Herm
