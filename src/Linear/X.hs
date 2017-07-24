module Linear.X where

import Algebra

data X a where
  Xd :: {-# UNPACK #-} !Double -> X Double
  Xz :: {-# UNPACK #-} !(Complex Double) -> X (Complex Double)

unX :: X a -> a
unX (Xd a) = a
unX (Xz a) = a

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

instance Division (X Double) where
  recip (Xd a) = Xd (recip a)
  (/) (Xd a) (Xd b) = Xd (a / b)

instance Division (X (Complex Double)) where
  recip (Xz a) = Xz (recip a)
  (/) (Xz a) (Xz b) = Xz (a / b)

instance LeftModule Integer (X a) where
  (.*) n (Xd a) = Xd (fromInteger n * a)
  (.*) n (Xz a) = Xz (fromInteger n * a)

instance RightModule Integer (X a) where
  (*.) (Xd a) n = Xd (a * fromInteger n)
  (*.) (Xz a) n = Xz (a * fromInteger n)

instance Group (X Double) where
  negate (Xd a) = Xd (negate a)

instance Group (X (Complex Double)) where
  negate (Xz a) = Xz (negate a)

instance Semiring (X Double)

instance Rig (X Double) where
  fromNatural n = Xd (fromNatural n)

instance Ring (X Double) where
  fromInteger n = Xd (fromInteger n)

instance Semiring (X (Complex Double))

instance Rig (X (Complex Double)) where
  fromNatural n = Xz (fromNatural n)

instance Ring (X (Complex Double)) where
  fromInteger n = Xz (fromInteger n)
