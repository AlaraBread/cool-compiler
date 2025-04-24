module Cfg where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Util

data Cfg s v = Cfg
  { -- a map from labels to blocks in the Cfg
    cfgBlocks :: Map.Map Label [s],
    -- a map from labels to child blocks
    cfgChildren :: Map.Map Label (Set.Set Label),
    -- a map from labels to variables which are contained in them
    cfgVariables :: Map.Map Label (Set.Set v)
  }

-- this will reverse the edges and also reverse the statements in cfgBlocks
reverseCfg :: Cfg s v -> Cfg s v
reverseCfg = undefined

class (Eq a) => Lattice a where
  top :: a
  bottom :: a
  meet :: a -> a -> a
  join :: a -> a -> a -- aka lub
  (<=) :: a -> a -> Bool
  (<=) a b = join a b == b

-- s: statement type
-- v: underlying variable type
-- a: abstract value type (Lattice)
class (Lattice a) => Ai s v a where
  -- initial estimates, the statement, new estimates and the variables we touched
  transferFunction :: Map.Map v a -> s -> (Map.Map v a, Set.Set v)

invertMap :: Map.Map k (Set.Set v) -> Map.Map v (Set.Set k)
invertMap = undefined

-- Map.fromList $ map (,bottom) $ concat $ Map.elems $ cfgVariables cfg
buildInitialEstimates :: (Lattice a, Ord v) => Cfg s v -> Map.Map v a
buildInitialEstimates cfg = undefined

initialWorkList :: Cfg s v -> Set.Set Label
initialWorkList cfg = Set.fromList $ Map.keys $ cfgBlocks cfg

runAi' :: Set.Set Label -> Cfg s v -> Map.Map v a -> Map.Map v (Set.Set Label) -> (Set.Set Label, Map.Map v a)
runAi' workList cfg estimates variableMap =
  let (label, workList) = Set.deleteFindMin workList
      statements = Map.lookup label (cfgBlocks cfg)
   in if null workList
        then (Set.empty, estimates)
        else undefined -- TODO: actually recurse...

runAi :: (Ai s v a, Lattice a, Ord v) => Cfg s v -> Map.Map v a
runAi cfg =
  let workList = initialWorkList cfg
      estimates = buildInitialEstimates cfg
      variableMap = invertMap $ cfgVariables cfg
   in snd $ runAi' workList cfg estimates variableMap
