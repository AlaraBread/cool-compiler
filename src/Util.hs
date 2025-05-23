-- Simple utility function/types. If there is a file named Util, it is where
-- code that has no clear place goes to die :).
module Util where

import Control.Monad.State (MonadState (state), State)
import Data.Foldable (foldl')
import Data.Int (Int32)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

-- So we can keep track of line numbers.
data Lined a = Lined {line :: !Int, item :: !a}
  deriving (Eq)

instance Functor Lined where
  fmap f (Lined line item) = Lined line $ f item

instance Foldable Lined where
  foldMap f (Lined _ item) = f item

instance Traversable Lined where
  traverse f (Lined line item) = Lined line <$> f item

instance (Show a) => Show (Lined a) where
  show (Lined _ x) = show x

showLines :: (Show a) => [a] -> String
showLines trac = unlines (map show trac)

-- some basic newtypes
newtype Label = Label String
  deriving (Eq, Ord)

instance Show Label where
  show (Label l) = l

newtype Type = Type String
  deriving (Eq, Ord, Show)

-- label count (global), temporary count (local)
data Temporary = Temporary Int Int
  deriving (Show)

-- keep track of numbers
data Variable = TemporaryV Int | ParameterV Int | AttributeV Int
  deriving (Eq, Ord)

instance Show Variable where
  show (TemporaryV t) = "temp#" ++ show t
  show (AttributeV i) = "attribute#" ++ show i
  show (ParameterV i) = "parameter#" ++ show i

getVariable :: State Temporary Variable
getVariable = state $ \(Temporary l t) -> (TemporaryV $ t + 1, Temporary l $ t + 1)

getLabel :: State Temporary Label
getLabel = state $ \(Temporary l t) -> (Label $ "l" ++ show (l + 1), Temporary (l + 1) t)

reverseMap :: (Ord a, Ord b) => Map.Map a (Set.Set b) -> Map.Map b (Set.Set a)
reverseMap =
  Map.foldlWithKey'
    ( \m k v ->
        foldl'
          ( \m' v' ->
              Map.insert
                v'
                (Set.insert k $ fromMaybe Set.empty $ Map.lookup v' m)
                m'
          )
          m
          v
    )
    Map.empty

type TypeDetailsMap = Map.Map Type TypeDetails

-- Type size is in words.
data TypeDetails = TypeDetails {typeTag :: Int, typeSize :: Int, methodTags :: Map.Map String Int}
  deriving (Show, Eq)

-- div truncates towards negative infinity; this does not match cool semantics.
divTruncateTowardsZero :: Int32 -> Int32 -> Int32
divTruncateTowardsZero num den
  | signum num == signum den = div (abs num) (abs den)
  | otherwise = -(div (abs num) (abs den))
