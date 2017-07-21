module Linear
    ( X, asEltOfM
    , V
    , M
    , (|||), (===), (##), (#>)
    , konst, inv, tr, projective
    , module GHC.TypeLits
    ) where

import Data.Proxy
import qualified Data.Vector.Storable as V
import GHC.TypeLits

import Numeric.LinearAlgebra (Complex, Matrix, Vector)
import qualified Numeric.LinearAlgebra as L

data X a where
  Xd :: {-# UNPACK #-} !Double -> X Double
  Xz :: {-# UNPACK #-} !(Complex Double) -> X (Complex Double)

asEltOfM :: (Num a => a) -> M i j a -> X a
asEltOfM mk mat =
  case mat of
    Md _ -> Xd mk
    Mz _ -> Xz mk

data V (n :: Nat) a where
  Vd :: {-# UNPACK #-} !(Vector Double) -> V n Double
  Vz :: {-# UNPACK #-} !(Vector (Complex Double)) -> V n (Complex Double)

data M (m :: Nat) (n :: Nat) a where
  Md :: {-# UNPACK #-} !(Matrix Double) -> M m n Double
  Mz :: {-# UNPACK #-} !(Matrix (Complex Double)) -> M m n (Complex Double)

(|||) :: M i j a -> M i k a -> M i (j + k) a
(|||) (Md left) (Md right) = Md (left L.||| right)
(|||) (Mz left) (Mz right) = Mz (left L.||| right)

(===) :: M i j a -> M k j a -> M (i + k) j a
(===) (Md top) (Md bot) = Md (top L.=== bot)
(===) (Mz top) (Mz bot) = Mz (top L.=== bot)

rows :: forall a (i :: Nat) (j :: Nat). KnownNat i => M i j a -> Int
rows _ =
  let
    p = Proxy :: Proxy i
  in
    fromEnum (natVal p)

cols :: forall a (i :: Nat) (j :: Nat). KnownNat j => M i j a -> Int
cols _ =
  let
    p = Proxy :: Proxy j
  in
    fromEnum (natVal p)

konst :: (KnownNat i, KnownNat j, Num a) => X a -> M i j a
konst x =
  let
    self =
      case x of
        Xd c -> Md (L.konst c (rows self, cols self))
        Xz c -> Mz (L.konst c (rows self, cols self))
  in
    self

inv :: M n n a -> M n n a
inv (Md m) = Md (L.inv m)
inv (Mz m) = Mz (L.inv m)

tr :: M i j a -> M j i a
tr (Md m) = Md (L.tr m)
tr (Mz m) = Mz (L.tr m)

(##) :: M i j a -> M j k a -> M i k a
(##) (Md a) (Md b) = Md (a L.<> b)
(##) (Mz a) (Mz b) = Mz (a L.<> b)

(#>) :: M i j a -> V j a -> V i a
(#>) (Md m) (Vd v) = Vd (m L.#> v)
(#>) (Mz m) (Vz v) = Vz (m L.#> v)

projective :: V i a -> V (i + 1) a
projective (Vd a) = Vd (V.cons 1 a)
projective (Vz a) = Vz (V.cons 1 a)
