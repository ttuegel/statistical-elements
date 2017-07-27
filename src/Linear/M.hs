{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Linear.M where

import Prelude.Local
import qualified Data.Vector.Storable as V
import GHC.TypeLits
import qualified Prelude as P

import Numeric.LinearAlgebra (Herm, Matrix)
import qualified Numeric.LinearAlgebra as L

import Algebra
import Linear.X
import Linear.V

data M (m :: Nat) (n :: Nat) a where
  Md :: {-# UNPACK #-} !(Matrix Double) -> M m n Double
  Mz :: {-# UNPACK #-} !(Matrix (Complex Double)) -> M m n (Complex Double)

instance Additive (M m n a) where
  (+) (Md a) (Md b) = Md (a P.+ b)
  (+) (Mz a) (Mz b) = Mz (a P.+ b)

instance Semiring a => LeftModule a (M m n a) where
  (.*) a (Md m) = Md (L.scale a m)
  (.*) a (Mz m) = Mz (L.scale a m)

instance Semiring a => RightModule a (M m n a) where
  (*.) (Md m) a = Md (L.scale a m)
  (*.) (Mz m) a = Mz (L.scale a m)

instance LeftModule Natural (M m n a) where
  (.*) n (Md m) = Md (L.cmap (n .*) m)
  (.*) n (Mz m) = Mz (L.cmap (n .*) m)

instance RightModule Natural (M m n a) where
  (*.) (Md m) n = Md (L.cmap (*. n) m)
  (*.) (Mz m) n = Mz (L.cmap (*. n) m)

instance (KnownNat m, KnownNat n) => Monoidal (M m n Double) where
  zero = konst zero

instance (KnownNat m, KnownNat n) => Monoidal (M m n (Complex Double)) where
  zero = konst zero

instance LeftModule Integer (M m n a) where
  (.*) n (Md m) = Md (L.cmap (n .*) m)
  (.*) n (Mz m) = Mz (L.cmap (n .*) m)

instance RightModule Integer (M m n a) where
  (*.) (Md m) n = Md (L.cmap (*. n) m)
  (*.) (Mz m) n = Mz (L.cmap (*. n) m)

instance (KnownNat m, KnownNat n) => Group (M m n Double)

instance (KnownNat m, KnownNat n) => Group (M m n (Complex Double))

data H (n :: Nat) a where
  Hd :: {-# UNPACK #-} !(Herm Double) -> H n Double
  Hz :: {-# UNPACK #-} !(Herm (Complex Double)) -> H n (Complex Double)

instance Additive (H n a) where
  (+) (Hd a) (Hd b) = Hd (L.trustSym (L.unSym a P.+ L.unSym b))
  (+) (Hz a) (Hz b) = Hz (L.trustSym (L.unSym a P.+ L.unSym b))

instance LeftModule Natural (H n a) where
  (.*) n (Hd a) = Hd (L.trustSym (L.cmap (n .*) (L.unSym a)))
  (.*) n (Hz a) = Hz (L.trustSym (L.cmap (n .*) (L.unSym a)))

instance RightModule Natural (H n a) where
  (*.) (Hd a) n = Hd (L.trustSym (L.cmap (*. n) (L.unSym a)))
  (*.) (Hz a) n = Hz (L.trustSym (L.cmap (*. n) (L.unSym a)))

instance KnownNat n => Monoidal (H n Double) where
  zero =
    let
      self = Hd (L.trustSym (L.konst zero (n, n)))
      n = rows (unsym self)
    in
      self

instance KnownNat n => Monoidal (H n (Complex Double)) where
  zero =
    let
      self = Hz (L.trustSym (L.konst zero (n, n)))
      n = rows (unsym self)
    in
      self

instance LeftModule Integer (H n a) where
  (.*) n (Hd a) = Hd (L.trustSym (L.cmap (n .*) (L.unSym a)))
  (.*) n (Hz a) = Hz (L.trustSym (L.cmap (n .*) (L.unSym a)))

instance RightModule Integer (H n a) where
  (*.) (Hd a) n = Hd (L.trustSym (L.cmap (*. n) (L.unSym a)))
  (*.) (Hz a) n = Hz (L.trustSym (L.cmap (*. n) (L.unSym a)))

instance KnownNat n => Group (H n Double)

instance KnownNat n => Group (H n (Complex Double))

asEltOfM :: (Semiring a => a) -> M i j a -> X a
asEltOfM mk mat =
  case mat of
    Md _ -> Xd mk
    Mz _ -> Xz mk

unsym :: H n a -> M n n a
unsym (Hd m) = Md (L.unSym m)
unsym (Hz m) = Mz (L.unSym m)

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

konst :: (KnownNat i, KnownNat j) => X a -> M i j a
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

projectiveV :: V i a -> V (i + 1) a
projectiveV (Vd a) = Vd (V.cons 1 a)
projectiveV (Vz a) = Vz (V.cons 1 a)

projectiveM :: forall a (i :: Nat) (j :: Nat).
               KnownNat j =>
               M i j a -> M (i + 1) j a
projectiveM m@(Md _) = (konst one :: M 1 j Double) === m
projectiveM m@(Mz _) = (konst one :: M 1 j (Complex Double)) === m

sym :: M n n a -> H n a
sym (Md m) = Hd (L.sym m)
sym (Mz m) = Hz (L.sym m)

contract :: M m n a -> H n a
contract (Md m) = Hd (L.mTm m)
contract (Mz m) = Hz (L.mTm m)

det :: M n n a -> a
det (Md m) = L.det m
det (Mz m) = L.det m

takeDiag :: M n n a -> V n a
takeDiag (Md m) = Vd (L.takeDiag m)
takeDiag (Mz m) = Vz (L.takeDiag m)

toColumns :: M m n a -> [V m a]
toColumns (Md m) = Vd <$> L.toColumns m
