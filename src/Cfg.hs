{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Cfg where

import Data.Foldable (find)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified InputIr
import TracIr (TracStatement (..))
import qualified TracIr
import Util

data CfgIr s v = CfgIr
  { implementationMap :: Map.Map Type [InputIr.ImplementationMapEntry (CfgMethod s v)],
    typeDetailsMap :: TypeDetailsMap
  }
  deriving (Show, Eq)

data CfgMethod s v = CfgMethod {methodName :: String, body :: Cfg s v, formals :: [InputIr.Formal], temporaryCount :: Int}
  deriving (Show, Eq)

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
        Return a -> Cfg.ReturnStatement a
        Comment {} -> Cfg.OtherStatement Set.empty Set.empty
        ConditionalJump _ l l2 -> Cfg.ConditionalJumpStatement l l2
        Assign a b -> unary a b
        Case a b labels afterLabel -> Cfg.CaseStatement (Set.singleton a) (Set.singleton b) (map snd $ Map.toList labels) afterLabel
        TracInternal {} -> Cfg.OtherStatement Set.empty Set.empty
        Abort {} -> Cfg.AbortStatement

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

instance (Eq s, Eq v) => Eq (Cfg s v) where
  (==) (Cfg start blocks children preds vars defs) (Cfg start' blocks' children' preds' vars' defs') = start == start' && blocks == blocks' && children == children' && preds == preds' && vars == vars' && defs == defs'

-- this will reverse all of the edges and also reverse the statements in
-- cfgBlocks, except for the first statement [ which should be the label ]
reverseCfg :: Cfg s v -> Cfg s v
reverseCfg cfg =
  cfg
    { cfgChildren = reverseMap $ cfgPredecessors cfg,
      cfgPredecessors = reverseMap $ cfgChildren cfg,
      cfgBlocks = Map.map (\statements -> head statements : reverse (tail statements)) $ cfgBlocks cfg
    }

data CfgStatementType v
  = JumpStatement Label
  | ConditionalJumpStatement Label Label
  | LabelStatement Label
  | CaseStatement (Set.Set v) (Set.Set v) [Label] Label
  | ReturnStatement v
  | AbortStatement
  | -- defined used
    OtherStatement (Set.Set v) (Set.Set v)

constructCfg :: (Ord v) => (s -> CfgStatementType v) -> [Lined s] -> Cfg s v
constructCfg getStatementType (firstStatement : statements) =
  -- crash if there isnt a label at the start of the list
  let Lined lineNumber (LabelStatement firstLabel) = getStatementType <$> firstStatement
      getLabel :: CfgStatementType v -> Maybe Label
      getLabel (LabelStatement label) = Just label
      getLabel _ = Nothing
      labels = firstLabel : mapMaybe (getLabel . getStatementType . item) statements
      (cfg, _, _) =
        List.foldl'
          (constructCfg' getStatementType)
          ( Cfg
              firstLabel
              (Map.singleton firstLabel [firstStatement])
              (Map.fromList $ map (,Set.empty) labels) -- ensure we always have a children set
              (Map.fromList $ map (,Set.empty) labels) -- ensure we always have a predecessors set
              Map.empty
              Map.empty,
            firstLabel,
            Constructing
          )
          statements
   in cfg
constructCfg _ [] = undefined -- crash on empty list

-- allows us to skip to the next label after an abort or return statement
data CfgState = Skipping | Constructing

constructCfg' :: (Ord v) => (s -> CfgStatementType v) -> (Cfg s v, Label, CfgState) -> Lined s -> (Cfg s v, Label, CfgState)
constructCfg' getStatementType (Cfg startLabel blocks children predecessors variables variablesDefined, currentLabel, state) statement =
  let insertIntoChildren parent childList =
        Map.insert
          parent
          ( Set.fromList childList
              <> fromMaybe Set.empty (Map.lookup currentLabel children)
          )
      insertIntoBlock label =
        Map.insert
          label
          ( fromMaybe
              []
              (Map.lookup label blocks)
              ++ [statement]
          )
          blocks
      blocks' = insertIntoBlock currentLabel
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
   in case state of
        Skipping -> case getStatementType statement' of
          LabelStatement label ->
            ( Cfg startLabel (insertIntoBlock label) children predecessors variables variablesDefined,
              label,
              if currentLabel == label then Skipping else Constructing
            )
          _ ->
            ( Cfg startLabel blocks children predecessors variables variablesDefined,
              currentLabel,
              Skipping
            )
        Constructing -> case getStatementType statement' of
          JumpStatement label ->
            ( Cfg
                startLabel
                blocks'
                (insertIntoChildren currentLabel [label] children)
                (insertIntoPredecessors currentLabel [label] predecessors)
                variables
                variablesDefined,
              currentLabel,
              Skipping
            )
          ConditionalJumpStatement l1 l2 ->
            ( Cfg
                startLabel
                blocks'
                (insertIntoChildren currentLabel [l1, l2] children)
                (insertIntoPredecessors currentLabel [l1, l2] predecessors)
                variables
                variablesDefined,
              currentLabel,
              Constructing
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
              currentLabel,
              Constructing
            )
          LabelStatement label ->
            if currentLabel == label
              then (Cfg startLabel blocks' children predecessors variables variablesDefined, label, Constructing)
              else
                ( Cfg
                    startLabel
                    (insertIntoBlock label)
                    (insertIntoChildren currentLabel [label] children)
                    (insertIntoPredecessors currentLabel [label] predecessors)
                    variables
                    variablesDefined,
                  label,
                  Constructing
                )
          ReturnStatement a ->
            ( Cfg
                startLabel
                blocks'
                children
                predecessors
                (insertIntoVariables (Set.singleton a) variables)
                variablesDefined,
              currentLabel,
              Skipping
            )
          AbortStatement ->
            ( Cfg
                startLabel
                blocks'
                children
                predecessors
                variables
                variablesDefined,
              currentLabel,
              Skipping
            )
          OtherStatement defined used ->
            ( Cfg
                startLabel
                blocks'
                children
                predecessors
                (insertIntoVariables used variables)
                (insertIntoVariables defined variablesDefined),
              currentLabel,
              Constructing
            )

cfgToLinearCode :: (Ord v) => (TracStatement v -> CfgStatementType v) -> Cfg (TracStatement v) v -> [Lined (TracStatement v)]
cfgToLinearCode getStatementType = cfgToLinearCode' getStatementType . insertJumps

-- insert jumps where we have blocks with one child
insertJumps :: (Ord v) => Cfg (TracStatement v) v -> Cfg (TracStatement v) v
insertJumps (Cfg start blocks children preds vars defs) =
  let blocks' =
        Map.mapWithKey
          ( \label block ->
              if length (children Map.! label) == 1
                then block ++ [Lined 0 (Jump $ Set.findMin $ children Map.! label)]
                else block
          )
          blocks
   in Cfg start blocks children preds vars defs

-- we should probably do something kinder to the branch predictor. oops. at
-- least the labels are vaguely in the order of the original code?
--
-- entry point is first because class names are capitalized so Class.method will
-- be before lN. locked in.

cfgToLinearCode' :: (Ord v) => (s -> CfgStatementType v) -> Cfg s v -> [Lined s]
cfgToLinearCode' getStatementType (Cfg start blocks children preds vars defs) =
  let isConditionalJump (ConditionalJumpStatement _ _) = True
      isConditionalJump _ = False

      getFallthrough (ConditionalJumpStatement _ fallthrough) = fallthrough

      issueBlock = fst <$> find (\(_, block) -> isConditionalJump $ getStatementType $ item $ last block) (Map.toList blocks)
   in case issueBlock of
        Just block ->
          let fallthrough = getFallthrough $ getStatementType $ item $ last $ blocks Map.! block
              correctedCode = blocks Map.! block ++ blocks Map.! fallthrough
              blocks' = Map.delete fallthrough $ Map.insert block correctedCode blocks
           in cfgToLinearCode' getStatementType $ Cfg start blocks' children preds vars defs
        Nothing -> concat $ Map.elems blocks

-- cfgToLinearCode :: (s -> CfgStatementType v) -> Cfg s v -> [Lined s]
-- cfgToLinearCode getStatementType cfg =
--   -- ensure fallthrough blocks are in correct order
--   let (blocks', _) =
--         foldl
--           ( \(blocks, visited) (block, blockContents) ->
--               if Set.member block visited
--                 then (blocks, visited)
--                 else
--                   let Lined _ lastStatement = last blockContents
--                    in case getStatementType lastStatement of
--                         ConditionalJumpStatement _ fallthrough ->
--                           ( Map.delete fallthrough $
--                               Map.insert
--                                 block
--                                 (blockContents ++ (cfgBlocks cfg Map.! fallthrough))
--                                 blocks,
--                             Set.fromList [block, fallthrough] <> visited
--                           )
--                         _ -> (Map.insert block blockContents blocks, Set.insert block visited)
--           )
--           (Map.empty, Set.empty)
--           (Map.toList $ cfgBlocks cfg)
--    in concat (Map.elems blocks')

cfgMethodToLinearCode :: (Ord v) => CfgMethod (TracStatement v) v -> TracIr.TracMethod v
cfgMethodToLinearCode (CfgMethod methodName body formals temporaryCount) =
  TracIr.TracMethod methodName (cfgToLinearCode getTracStatementType body) formals temporaryCount

-- neat debugging tool
cfgToGraphviz :: Cfg s v -> String
cfgToGraphviz (Cfg {cfgChildren}) =
  "digraph {\n"
    ++ concat
      ( concatMap
          snd
          ( Map.toList $
              Map.mapWithKey
                (\parent -> Set.toList . Set.map (\child -> "  \"" ++ show parent ++ "\" -> \"" ++ show child ++ "\";\n"))
                cfgChildren
          )
      )
    ++ "}"

-- We add the estimate before and after a given program point to our cfg
type AnnotatedCfg s v a = Cfg (s, Map.Map v (a, a)) v

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

buildInitialEstimates :: (Lattice a, Ord v) => Cfg s v -> Map.Map v (a, a)
buildInitialEstimates cfg =
  Map.fromList $
    map (,(bottom, bottom)) $
      concatMap Set.toList $
        Map.elems $
          cfgVariables cfg

buildInitialEstimatesCfg :: (Lattice a, Ord v) => Cfg s v -> AnnotatedCfg s v a
buildInitialEstimatesCfg cfg =
  let blocks = cfgBlocks cfg
      initialEstimates = buildInitialEstimates cfg
      blocks' = fmap (fmap (fmap (,initialEstimates))) blocks
   in cfg {cfgBlocks = blocks'}

initialWorkList :: Cfg s v -> Set.Set Label
initialWorkList cfg = Set.fromList $ Map.keys $ cfgBlocks cfg

runAiStep ::
  (Ai s v a, Ord v) =>
  Map.Map v (Set.Set Label) -> -- variable map
  ([Lined (s, Map.Map v (a, a))], Map.Map v a, Set.Set Label) -> -- processed statements, before state, worklist
  Lined (s, Map.Map v (a, a)) -> -- statement
  ([Lined (s, Map.Map v (a, a))], Map.Map v a, Set.Set Label) -- processed statements, after state, worklist
runAiStep variableMap (processedStatements, beforeState, workList) (Lined line (statement, _)) =
  let (afterState, affectedVariables) = transferFunction beforeState statement
      affectedBlocks = Set.unions $ Set.map (variableMap Map.!) affectedVariables
      workList' = Set.union workList affectedBlocks
      beforeAfterState = Map.intersectionWith (,) beforeState afterState
      statement' = Lined line (statement, beforeAfterState)
   in (statement' : processedStatements, afterState, workList')

runAi' ::
  (Ai s v a, Ord v) =>
  Set.Set Label ->
  AnnotatedCfg s v a ->
  Map.Map v (Set.Set Label) ->
  Map.Map v a ->
  (AnnotatedCfg s v a, Set.Set Label)
runAi' workList cfg variableMap bottomEstimates =
  let (label, workList') = Set.deleteFindMin workList
      statements = cfgBlocks cfg Map.! label
      -- we compute the initial state by taking the lub of the end estimates of
      -- all of the predecessors and the estimate of all bottoms, in case there
      -- are no predecessors.
      predecessors = Set.toList $ cfgPredecessors cfg Map.! label
      predEndEstimates = fmap (fmap snd . snd . item . last . (cfgBlocks cfg Map.!)) predecessors
      initialState = Map.unionsWith join (bottomEstimates : predEndEstimates)
      (statementsReverse', _, workList'') =
        List.foldl'
          (runAiStep variableMap)
          ([], initialState, workList')
          statements
      statements' = reverse statementsReverse'
      cfgBlocks' = Map.insert label statements' (cfgBlocks cfg)
   in if null workList
        then (cfg, Set.empty)
        else runAi' workList'' (cfg {cfgBlocks = cfgBlocks'}) variableMap bottomEstimates

-- we add a set of estimates after every statement. this covers every program
-- point because all blocks start with a Label.
runAi :: (Ai s v a, Lattice a, Ord v) => Cfg s v -> AnnotatedCfg s v a
runAi cfg =
  let workList = initialWorkList cfg
      bottomEstimates = snd <$> buildInitialEstimates cfg
      cfg' = buildInitialEstimatesCfg cfg
      variableMap = invertMap $ cfgVariables cfg
   in fst $ runAi' workList cfg' variableMap bottomEstimates

removeAnnotations :: AnnotatedCfg s v a -> Cfg s v
removeAnnotations cfg = cfg {cfgBlocks = Map.map (fmap $ fmap fst) $ cfgBlocks cfg}

-- we will overallocate if we decrease the number of temporaries in
-- optimization. if we increase have a bad day.
mapCfgMethod :: (Cfg s1 v1 -> Cfg s2 v2) -> CfgMethod s1 v1 -> CfgMethod s2 v2
mapCfgMethod f (CfgMethod methodName body formals temporaryCount) =
  CfgMethod methodName (f body) formals temporaryCount

mapCfgIr :: (Cfg s1 v1 -> Cfg s2 v2) -> CfgIr s1 v1 -> CfgIr s2 v2
mapCfgIr f (CfgIr implMap typeDetailsMap) =
  CfgIr (fmap (fmap (fmap (mapCfgMethod f))) implMap) typeDetailsMap
