{-# OPTIONS_GHC -fno-warn-orphans #-}

module Algebra
    ( module Data.Complex
    , module Numeric.Algebra
    , module Numeric.Ring.Class
    ) where

import Prelude.Local

import Data.Complex (Complex(..))
import Numeric.Algebra hiding (InvolutiveAlgebra(..), Order(..))
import Numeric.Ring.Class
import qualified Prelude as P

instance Additive Double where
  (+) = (P.+)

instance Abelian Double

instance LeftModule Natural Double where
  (.*) n a = fromNatural n * a

instance RightModule Natural Double where
  (*.) a n = a * fromNatural n

instance Monoidal Double where
  zero = 0.0

instance Ring Double

instance LeftModule Integer Double where
  (.*) n a = fromInteger n * a

instance RightModule Integer Double where
  (*.) a n = a * fromInteger n

instance Group Double

instance Multiplicative Double where
  (*) = (P.*)

instance Semiring Double

instance Unital Double where
  one = 1.0

instance Division Double

instance Rig Double

instance Additive a => Additive (Complex a) where
  (+) (ua :+ ub) (va :+ vb) = (ua + va) :+ (ub + vb)

instance Abelian a => Abelian (Complex a)

instance LeftModule Natural a => LeftModule Natural (Complex a) where
  (.*) n (a :+ b) = (n .* a) :+ (n .* b)

instance RightModule Natural a => RightModule Natural (Complex a) where
  (*.) (a :+ b) n = (a *. n) :+ (b *. n)

instance Monoidal a => Monoidal (Complex a) where
  zero = zero :+ zero

instance LeftModule Integer a => LeftModule Integer (Complex a) where
  (.*) n (a :+ b) = (n .* a) :+ (n .* b)

instance RightModule Integer a => RightModule Integer (Complex a) where
  (*.) (a :+ b) n = (a *. n) :+ (b *. n)

instance Group a => Group (Complex a)

instance (Group a, Multiplicative a) => Multiplicative (Complex a) where
  (*) (ua :+ ub) (va :+ vb) = (ua * va - ub * vb) :+ (ub * va + ua * vb)

instance (Group a, Semiring a) => Semiring (Complex a)

instance (Group a, Rig a) => Rig (Complex a)

instance (Group a, Unital a) => Unital (Complex a) where
  one = one :+ zero

instance Division (Complex Double)

instance Ring a => Ring (Complex a)
