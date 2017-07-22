module Linear.X where

import Algebra

data X a where
  Xd :: {-# UNPACK #-} !Double -> X Double
  Xz :: {-# UNPACK #-} !(Complex Double) -> X (Complex Double)

instance Additive (X a) where
  (+) (Xd a) (Xd b) = Xd (a + b)
  (+) (Xz a) (Xz b) = Xz (a + b)

instance Abelian (X a)

instance Monoidal (X Double) where
  zero = Xd 0

instance Monoidal (X (Complex Double)) where
  zero = Xz 0

instance LeftModule Natural (X a) where
  (.*) n (Xd a) = Xd (fromNatural n * a)
  (.*) n (Xz a) = Xz (fromNatural n * a)

instance RightModule Natural (X a) where
  (*.) (Xd a) n = Xd (a * fromNatural n)
  (*.) (Xz a) n = Xz (a * fromNatural n)

instance Unital (X Double) where
  one = Xd one

instance Unital (X (Complex Double)) where
  one = Xz one

instance Multiplicative (X a) where
  (*) (Xd a) (Xd b) = Xd (a * b)
  (*) (Xz a) (Xz b) = Xz (a * b)
