{-# LANGUAGE MultiParamTypeClasses #-}

module Cfg where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Util

data Cfg s v = Cfg
  { -- a map from labels to blocks in the Cfg
    cfgBlocks :: Map.Map Label [s],
    -- a map from labels to child blocks
    cfgChildren :: Map.Map Label (Set.Set Label),
    -- a map from labels to predecessor blocks
    cfgPredecessors :: Map.Map Label (Set.Set Label),
    -- a map from labels to variables which are used in them
    cfgVariables :: Map.Map Label (Set.Set v),
    -- map from labels to variables defined in them
    cfgDefinitions :: Map.Map Label (Set.Set v)
  }

-- this will reverse the edges and also reverse the statements in cfgBlocks
reverseCfg :: Cfg s v -> Cfg s v
reverseCfg = undefined

data CfgStatementType v
  = JumpStatement Label
  | ConditionalJumpStatement Label Label
  | LabelStatement Label
  | CaseStatement (Set.Set v) (Set.Set v) [Label] Label
  | -- defined used
    OtherStatement (Set.Set v) (Set.Set v)

constructCfg :: (Ord v) => (s -> CfgStatementType v) -> [s] -> Cfg s v
constructCfg getStatementType (firstStatement : statements) =
  -- crash if there isnt a label at the start of the list
  let LabelStatement firstLabel = getStatementType firstStatement
   in fst $
        foldl
          (constructCfg' getStatementType)
          (Cfg Map.empty Map.empty Map.empty Map.empty Map.empty, firstLabel)
          statements
constructCfg _ [] = undefined -- crash on empty list

constructCfg' :: (Ord v) => (s -> CfgStatementType v) -> (Cfg s v, Label) -> s -> (Cfg s v, Label)
constructCfg' getStatementType (Cfg blocks children predecessors variables variablesDefined, currentLabel) statement =
  let insertIntoChildren parent childList =
        Map.insert
          parent
          ( Set.fromList childList
              <> fromMaybe Set.empty (Map.lookup currentLabel children)
          )
      blocks' =
        Map.insert
          currentLabel
          ( fromMaybe
              []
              (Map.lookup currentLabel blocks)
              ++ [statement]
          )
          blocks
      insertIntoPredecessors parent childList p =
        foldl
          ( \m child ->
              Map.insert
                child
                ( Set.insert parent $
                    fromMaybe Set.empty (Map.lookup child m)
                )
                m
          )
          p
          childList
      insertIntoVariables vars =
        Map.insert
          currentLabel
          (vars <> fromMaybe Set.empty (Map.lookup currentLabel variables))
   in case getStatementType statement of
        JumpStatement label ->
          ( Cfg
              blocks'
              (insertIntoChildren currentLabel [label] children)
              (insertIntoPredecessors currentLabel [label] predecessors)
              variables
              variablesDefined,
            currentLabel
          )
        ConditionalJumpStatement l1 l2 ->
          ( Cfg
              blocks'
              (insertIntoChildren currentLabel [l1, l2] children)
              (insertIntoPredecessors currentLabel [l1, l2] predecessors)
              variables
              variablesDefined,
            currentLabel
          )
        CaseStatement defined used labels afterLabel ->
          ( Cfg
              blocks'
              ( foldl
                  (\c l -> insertIntoChildren l [afterLabel] c)
                  (insertIntoChildren currentLabel labels children)
                  labels
              )
              ( foldl
                  (\p l -> insertIntoPredecessors l [afterLabel] p)
                  (insertIntoPredecessors currentLabel labels predecessors)
                  labels
              )
              (insertIntoVariables used variables)
              (insertIntoVariables defined variablesDefined),
            currentLabel
          )
        LabelStatement label ->
          if currentLabel == label
            then (Cfg blocks' children predecessors variables variablesDefined, label)
            else
              ( Cfg
                  blocks'
                  (insertIntoChildren currentLabel [label] children)
                  (insertIntoPredecessors currentLabel [label] predecessors)
                  variables
                  variablesDefined,
                label
              )
        OtherStatement defined used ->
          ( Cfg
              blocks'
              children
              predecessors
              (insertIntoVariables used variables)
              (insertIntoVariables defined variablesDefined),
            currentLabel
          )

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
