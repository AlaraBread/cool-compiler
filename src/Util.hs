-- Simple utility function/types. If there is a file named Util, it is where
-- code that has no clear place goes to die :).
module Util where

import Control.Monad.State (MonadState (state), State)

-- So we can keep track of line numbers.
data Lined a = Lined {line :: !Int, item :: !a}

instance Functor Lined where
  fmap f (Lined line item) = Lined line $ f item

instance Foldable Lined where
  foldMap f (Lined _ item) = f item

instance Traversable Lined where
  traverse f (Lined line item) = Lined line <$> f item

instance (Show a) => Show (Lined a) where
  show (Lined _ x) = show x

unsequence :: (Traversable t) => Lined (t a) -> t (Lined a)
unsequence (Lined line x) = fmap (Lined line) x

showLines :: (Show a) => [a] -> String
showLines trac = unlines (map show trac)

-- some basic newtypes
newtype Label = Label String

newtype Type = Type String
  deriving (Eq, Ord, Show)

instance Show Label where
  show (Label l) = l

-- label count (global), temporary count (local)
data Temporary = Temporary Int Int
  deriving (Show)

-- keep track of numbers
data Variable = TemporaryV Int | ParameterV Int | AttributeV Int

instance Show Variable where
  show (TemporaryV t) = "temp#" ++ show t
  show (AttributeV i) = "attribute#" ++ show i
  show (ParameterV i) = "parameter#" ++ show i

getVariable :: State Temporary Variable
getVariable = state $ \(Temporary l t) -> (TemporaryV $ t + 1, Temporary l $ t + 1)

getLabel :: State Temporary Label
getLabel = state $ \(Temporary l t) -> (Label $ "l" ++ show (l + 1), Temporary (l + 1) t)
