module Exception
    ( module Control.Exception
    , ProbException(..)
    ) where

import Control.Exception
import Data.Typeable

data ProbException = ProbOverflow
  deriving (Show, Typeable)

instance Exception ProbException
