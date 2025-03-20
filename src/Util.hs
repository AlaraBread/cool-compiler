-- Simple utility function/types. If there is a file named Util, it is where
-- code that has no clear place goes to die :).
module Util where

-- So we can keep track of line numbers.
data Lined a = Lined {line :: !Int, item :: !a}

instance Functor Lined where
  fmap f (Lined line item) = Lined line $ f item

instance (Show a) => Show (Lined a) where
  show (Lined _ x) = show x
