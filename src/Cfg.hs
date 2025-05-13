{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Cfg where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified InputIr
import TracIr (TracStatement (..))
import qualified TracIr
import Util

data CfgIr s v = CfgIr
  { implementationMap :: Map.Map Type [InputIr.ImplementationMapEntry (CfgMethod s v)],
    typeDetailsMap :: TypeDetailsMap
  }
  deriving (Show)

data CfgMethod s v = CfgMethod {methodName :: String, body :: Cfg s v, formals :: [InputIr.Formal], temporaryCount :: Int}
  deriving (Show)

createCfgIr :: (Ord v) => TracIr.TracIr v -> CfgIr (TracStatement v) v
createCfgIr TracIr.TracIr {TracIr.implementationMap, TracIr.typeDetailsMap} =
  CfgIr
    { implementationMap =
        Map.map
          ( map $
              fmap $
                \TracIr.TracMethod
                   { TracIr.methodName,
                     TracIr.body,
                     TracIr.formals,
                     TracIr.temporaryCount
                   } ->
                    CfgMethod
                      { methodName,
                        body = constructCfg getTracStatementType body,
                        formals,
                        temporaryCount
                      }
          )
          implementationMap,
      typeDetailsMap
    }

getTracStatementType :: (Ord v) => TracStatement v -> CfgStatementType v
getTracStatementType s =
  let binary a b c = Cfg.OtherStatement (Set.singleton a) (Set.fromList [b, c])
      unary a b = Cfg.OtherStatement (Set.singleton a) (Set.singleton b)
      constant a = Cfg.OtherStatement (Set.singleton a) Set.empty
   in case s of
        Add a b c -> binary a b c
        Subtract a b c -> binary a b c
        Multiply a b c -> binary a b c
        Divide a b c -> binary a b c
        LessThan a b c -> binary a b c
        LessThanOrEqualTo a b c -> binary a b c
        Equals a b c -> binary a b c
        IntConstant a _ -> constant a
        BoolConstant a _ -> constant a
        StringConstant a _ -> constant a
        Not a b -> unary a b
        Negate a b -> unary a b
        New a _ -> constant a
        Default a _ -> constant a
        IsVoid a b -> unary a b
        Dispatch {dispatchResult, dispatchArgs, dispatchReceiver} ->
          Cfg.OtherStatement (Set.singleton dispatchResult) (Set.fromList $ dispatchReceiver : dispatchArgs)
        Jump l -> Cfg.JumpStatement l
        TracLabel l -> Cfg.LabelStatement l
        Return a -> Cfg.OtherStatement Set.empty (Set.singleton a)
        Comment {} -> Cfg.OtherStatement Set.empty Set.empty
        ConditionalJump _ l l2 -> Cfg.ConditionalJumpStatement l l2
        Assign a b -> unary a b
        Case a b labels afterLabel -> Cfg.CaseStatement (Set.singleton a) (Set.singleton b) (map snd $ Map.toList labels) afterLabel
        TracInternal {} -> Cfg.OtherStatement Set.empty Set.empty
        Abort {} -> Cfg.OtherStatement Set.empty Set.empty

data Cfg s v = Cfg
  { cfgStart :: Label,
    -- a map from labels to blocks in the Cfg
    cfgBlocks :: Map.Map Label [Lined s],
    -- a map from labels to child blocks
    cfgChildren :: Map.Map Label (Set.Set Label),
    -- a map from labels to predecessor blocks
    cfgPredecessors :: Map.Map Label (Set.Set Label),
    -- a map from labels to variables which are used in them
    cfgVariables :: Map.Map Label (Set.Set v),
    -- map from labels to variables defined in them
    cfgDefinitions :: Map.Map Label (Set.Set v)
  }
  deriving (Show)

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

constructCfg :: (Ord v) => (s -> CfgStatementType v) -> [Lined s] -> Cfg s v
constructCfg getStatementType (firstStatement : statements) =
  -- crash if there isnt a label at the start of the list
  let Lined lineNumber (LabelStatement firstLabel) = getStatementType <$> firstStatement
   in fst $
        foldl
          (constructCfg' getStatementType)
          (Cfg firstLabel (Map.singleton firstLabel [firstStatement]) Map.empty Map.empty Map.empty Map.empty, firstLabel)
          statements
constructCfg _ [] = undefined -- crash on empty list

constructCfg' :: (Ord v) => (s -> CfgStatementType v) -> (Cfg s v, Label) -> Lined s -> (Cfg s v, Label)
constructCfg' getStatementType (Cfg startLabel blocks children predecessors variables variablesDefined, currentLabel) statement =
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
      insertIntoVariables newVars allVars =
        Map.insert
          currentLabel
          (newVars <> fromMaybe Set.empty (Map.lookup currentLabel allVars))
          allVars
      Lined _ statement' = statement
   in case getStatementType statement' of
        JumpStatement label ->
          ( Cfg
              startLabel
              blocks'
              (insertIntoChildren currentLabel [label] children)
              (insertIntoPredecessors currentLabel [label] predecessors)
              variables
              variablesDefined,
            currentLabel
          )
        ConditionalJumpStatement l1 l2 ->
          ( Cfg
              startLabel
              blocks'
              (insertIntoChildren currentLabel [l1, l2] children)
              (insertIntoPredecessors currentLabel [l1, l2] predecessors)
              variables
              variablesDefined,
            currentLabel
          )
        CaseStatement defined used labels afterLabel ->
          ( Cfg
              startLabel
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
            then (Cfg startLabel blocks' children predecessors variables variablesDefined, label)
            else
              ( Cfg
                  startLabel
                  blocks'
                  (insertIntoChildren currentLabel [label] children)
                  (insertIntoPredecessors currentLabel [label] predecessors)
                  variables
                  variablesDefined,
                label
              )
        OtherStatement defined used ->
          ( Cfg
              startLabel
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

invertMap :: (Ord k, Ord v) => Map.Map k (Set.Set v) -> Map.Map v (Set.Set k)
invertMap originalMap =
  Map.fromListWith Set.union $
    concatMap (\(k, set) -> (,Set.singleton k) <$> Set.toList set) $
      Map.toList originalMap

buildInitialEstimates :: (Lattice a, Ord v) => Cfg s v -> Map.Map v a
buildInitialEstimates cfg =
  Map.fromList $
    map (,bottom) $
      concatMap Set.toList $
        Map.elems $
          cfgVariables cfg

initialWorkList :: Cfg s v -> Set.Set Label
initialWorkList cfg = Set.fromList $ Map.keys $ cfgBlocks cfg

runAiStep :: (Ai s v a, Ord v) => Map.Map v (Set.Set Label) -> (Map.Map v a, Set.Set Label) -> s -> (Map.Map v a, Set.Set Label)
runAiStep variableMap (estimates, workList) statement =
  let (estimates', affectedVariables) = transferFunction estimates statement
   in (estimates', Set.unions $ Set.map (variableMap Map.!) affectedVariables)

runAi' :: (Ai s v a, Ord v, Ai (Lined s) v a) => Set.Set Label -> Cfg s v -> Map.Map v a -> Map.Map v (Set.Set Label) -> (Map.Map v a, Set.Set Label)
runAi' workList cfg estimates variableMap =
  let (label, workList') = Set.deleteFindMin workList
      statements = cfgBlocks cfg Map.! label
      (estimates', workList'') =
        List.foldl'
          (runAiStep variableMap)
          (estimates, workList')
          statements
   in if null workList
        then (estimates, Set.empty)
        else runAi' workList'' cfg estimates' variableMap

runAi :: (Ai s v a, Lattice a, Ord v, Ai (Lined s) v a) => Cfg s v -> Map.Map v a
runAi cfg =
  let workList = initialWorkList cfg
      estimates = buildInitialEstimates cfg
      variableMap = invertMap $ cfgVariables cfg
   in fst $ runAi' workList cfg estimates variableMap
