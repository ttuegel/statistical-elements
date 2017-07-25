module Linear.V where

import Prelude.Local

import qualified Data.Vector.Storable as V
import GHC.TypeLits
import qualified Prelude as P

import Numeric.LinearAlgebra (Vector)
import qualified Numeric.LinearAlgebra as L

import Algebra
import Linear.X

data V (n :: Nat) a where
  Vd :: {-# UNPACK #-} !(Vector Double) -> V n Double
  Vz :: {-# UNPACK #-} !(Vector (Complex Double)) -> V n (Complex Double)

asEltOfV :: (Semiring a => a) -> V n a -> X a
asEltOfV mk v =
  case v of
    Vd _ -> Xd mk
    Vz _ -> Xz mk

dim :: forall a (n :: Nat). KnownNat n => V n a -> Int
dim _ =
  let
    p = Proxy :: Proxy n
  in
    fromEnum (natVal p)

fromList :: KnownNat n => [X a] -> V n a
fromList xs =
  let
    as = unX <$> xs
    self =
      case xs of
        Xd _ : _ -> Vd (L.fromList (take n as))
        Xz _ : _ -> Vz (L.fromList (take n as))
    n = dim self
  in
    self

basis :: forall (n :: Nat). KnownNat n => [V n Double]
basis =
  let
    p = Proxy :: Proxy n
    n = fromEnum (natVal p)
    e i = Vd (L.assoc n 0 [(i, one)])
  in
    e <$> take n [0..]

instance Additive (V n a) where
  (+) (Vd a) (Vd b) = Vd (a P.+ b)
  (+) (Vz a) (Vz b) = Vz (a P.+ b)

instance LeftModule Natural a => LeftModule Natural (V n a) where
  (.*) n (Vd v) = Vd (L.cmap (n .*) v)
  (.*) n (Vz v) = Vz (L.cmap (n .*) v)

instance RightModule Natural (V n a) where
  (*.) (Vd v) n = Vd (L.cmap (*. n) v)
  (*.) (Vz v) n = Vz (L.cmap (*. n) v)

instance KnownNat n => Monoidal (V n Double) where
  zero = let self = Vd (V.replicate (dim self) zero) in self

instance KnownNat n => Monoidal (V n (Complex Double)) where
  zero = let self = Vz (V.replicate (dim self) zero) in self

instance LeftModule Integer a => LeftModule Integer (V n a) where
  (.*) n (Vd v) = Vd (L.cmap (n .*) v)
  (.*) n (Vz v) = Vz (L.cmap (n .*) v)

instance RightModule Integer (V n a) where
  (*.) (Vd v) n = Vd (L.cmap (*. n) v)
  (*.) (Vz v) n = Vz (L.cmap (*. n) v)

instance KnownNat n => Group (V n Double)

instance KnownNat n => Group (V n (Complex Double))

instance Semiring a => LeftModule a (V n a) where
  (.*) a (Vd v) = Vd (L.scale a v)
  (.*) a (Vz v) = Vz (L.scale a v)

instance Semiring a => RightModule a (V n a) where
  (*.) (Vd v) a = Vd (L.scale a v)
  (*.) (Vz v) a = Vz (L.scale a v)
