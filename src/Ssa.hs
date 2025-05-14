{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Ssa where

import Cfg (Cfg (..))
import Control.Monad (foldM, forM, unless)
import Control.Monad.State
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Foldable (foldl', traverse_)
import Data.List (minimumBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Traversable as Set
import Debug.Trace (trace, traceM, traceShow, traceShowId, traceShowM, traceStack)
import GHC.Base (compareInt)
import TracIr (AbortReason (..), TracStatement (..))
import Util (Label, Lined (Lined), Variable, reverseMap)

data SsaVariable = SsaVariable Variable Int
  deriving (Ord, Eq)

instance Show SsaVariable where
  show (SsaVariable v i) = show v ++ "_" ++ show i

generateSsa :: Cfg (TracStatement Variable) Variable -> Cfg (TracStatement SsaVariable) SsaVariable
generateSsa cfg =
  let domFrontiers = dominanceFrontiers cfg
      Cfg {cfgStart, cfgBlocks, cfgChildren, cfgPredecessors, cfgVariables, cfgDefinitions} = cfg
      domTree =
        Map.union
          (reverseMap $ Map.map Set.singleton $ idom cfg)
          (Map.map (const Set.empty) cfgBlocks)
      domTree' = Map.insert cfgStart (Set.delete cfgStart (domTree Map.! cfgStart)) domTree
      blocks = Map.keys cfgBlocks
      variables = traceShowId $ Map.unionWith Set.union cfgDefinitions cfgVariables
      revVariables = traceShowId $ reverseMap variables

      phiFunctions =
        Map.fromList $
          map
            (\block -> (block, calculatePhiFunctionsForBlock revVariables (variables Map.! (traceShowId block)) (Map.findWithDefault Set.empty block $ reverseMap domFrontiers)))
            blocks

      cfgBlocks' =
        evalState
          ( rename
              (fillVariablesWithZero cfgBlocks)
              domTree'
              phiFunctions
              Map.empty
              cfgStart
          )
          Map.empty
   in -- TODO: set variable usages
      Cfg cfgStart cfgBlocks' cfgChildren cfgPredecessors Map.empty Map.empty

calculatePhiFunctionsForBlock ::
  Map.Map Variable (Set.Set Label) ->
  Set.Set Variable ->
  Set.Set Label ->
  Map.Map Variable (Set.Set Label)
calculatePhiFunctionsForBlock revVariables usedVariables revDomFrontier =
  Map.fromList $
    filter ((> 1) . Set.size . snd) $ -- we only need a phi function if we have conflicting definitions -- we only need a phi function if we have conflicting definitions -- we only need a phi function if we have conflicting definitions -- we only need a phi function if we have conflicting definitions
    -- we only need a phi function if we have conflicting definitions
      map (\var -> (var, trace "arf" (Set.intersection (traceShowId revDomFrontier) $ traceShowId (Map.findWithDefault Set.empty var revVariables)))) $
        Set.toList usedVariables

fillVariablesWithZero :: Map.Map Label [Lined (TracStatement Variable)] -> Map.Map Label [Lined (TracStatement SsaVariable)]
fillVariablesWithZero =
  let ssaVariable x = SsaVariable x 0
      binary op a b c = op (ssaVariable a) (ssaVariable b) (ssaVariable c)
      unary op a b = op (ssaVariable a) (ssaVariable b)
      constant op a = op (ssaVariable a)
   in Map.map $ map $ fmap $ \case
        Add a b c -> binary Add a b c
        Subtract a b c -> binary Subtract a b c
        Multiply a b c -> binary Multiply a b c
        Divide a b c -> binary Divide a b c
        LessThan a b c -> binary LessThan a b c
        LessThanOrEqualTo a b c -> binary LessThanOrEqualTo a b c
        Equals a b c -> binary Equals a b c
        IntConstant a b -> constant IntConstant a b
        BoolConstant a b -> constant BoolConstant a b
        StringConstant a b -> constant StringConstant a b
        Not a b -> unary Not a b
        Negate a b -> unary Negate a b
        New a b -> constant New a b
        Default a b -> constant Default a b
        IsVoid a b -> unary IsVoid a b
        Dispatch a b c d e f -> Dispatch (ssaVariable a) (ssaVariable b) c d e (map ssaVariable f)
        Jump l -> Jump l
        TracLabel l -> TracLabel l
        Return a -> Return $ ssaVariable a
        Comment a -> Comment a
        ConditionalJump a b c -> ConditionalJump (ssaVariable a) b c
        Assign a b -> unary Assign a b
        Case a b labels afterLabel -> Case (ssaVariable a) (ssaVariable b) labels afterLabel
        TracInternal a -> TracInternal a
        Abort a reason ->
          Abort
            a
            ( case reason of
                DispatchOnVoid -> DispatchOnVoid
                StaticDispatchOnVoid -> StaticDispatchOnVoid
                CaseOnVoid -> CaseOnVoid
                CaseNoMatch v -> CaseNoMatch $ ssaVariable v
                DivisionByZero -> DivisionByZero
                SubstringOutOfRange -> SubstringOutOfRange
            )
        Phi _ _ -> error "Phi function should not occur in fillVariablesWithZero"

-- https://sites.cs.ucsb.edu/~yufeiding/cs293s/slides/293S_06_SSA.pdf
rename ::
  Map.Map Label [Lined (TracStatement SsaVariable)] ->
  Map.Map Label (Set.Set Label) ->
  Map.Map Label (Map.Map Variable (Set.Set Label)) ->
  Map.Map Label (Map.Map Variable SsaVariable) ->
  Label ->
  State (Map.Map Variable SsaVariable) (Map.Map Label [Lined (TracStatement SsaVariable)])
rename blocks domTree phiFunctions openingVars block = do
  let blockStatements = Map.findWithDefault [] block blocks
  blockStatements' <-
    mapM
      ( \statementIn ->
          let Lined lineNumber statement = statementIn
              lined' = Lined lineNumber
              binary op lhs a b = do
                a' <- currentDefinition' a
                b' <- currentDefinition' b
                lhs' <- genName' lhs
                pure $ lined' $ op lhs' a' b'
              unary op lhs a = do
                a' <- currentDefinition' a
                lhs' <- genName' lhs
                pure $ lined' $ op lhs' a'
              constant op lhs c = do
                lhs' <- genName' lhs
                pure $ lined' $ op lhs' c
           in case statement of
                Add lhs a b -> binary Add lhs a b
                Subtract lhs a b -> binary Subtract lhs a b
                Multiply lhs a b -> binary Multiply lhs a b
                Divide lhs a b -> binary Divide lhs a b
                LessThan lhs a b -> binary LessThan lhs a b
                LessThanOrEqualTo lhs a b -> binary LessThanOrEqualTo lhs a b
                Equals lhs a b -> binary Equals lhs a b
                IntConstant lhs i -> constant IntConstant lhs i
                BoolConstant lhs b -> constant BoolConstant lhs b
                StringConstant lhs s -> constant StringConstant lhs s
                Not lhs a -> unary Not lhs a
                Negate lhs a -> unary Negate lhs a
                New lhs t -> constant New lhs t
                Default lhs t -> constant Default lhs t
                IsVoid lhs a -> unary IsVoid lhs a
                Dispatch
                  { dispatchResult,
                    dispatchReceiver,
                    dispatchReceiverType,
                    dispatchType,
                    dispatchMethod,
                    dispatchArgs
                  } -> do
                    dispatchArgs' <- traverse currentDefinition' dispatchArgs
                    dispatchReceiver' <- currentDefinition' dispatchReceiver
                    dispatchResult' <- genName' dispatchResult
                    pure $
                      lined' $
                        Dispatch
                          { dispatchResult = dispatchResult',
                            dispatchReceiver = dispatchReceiver',
                            dispatchReceiverType,
                            dispatchType,
                            dispatchMethod,
                            dispatchArgs = dispatchArgs'
                          }
                Jump l -> pure $ lined' $ Jump l
                TracLabel l -> pure $ lined' $ TracLabel l
                Return a -> do
                  a' <- currentDefinition' a
                  pure $ lined' $ Return a'
                Comment s -> pure $ lined' $ Comment s
                ConditionalJump condition trueLabel falseLabel -> do
                  condition' <- currentDefinition' condition
                  pure $ lined' $ ConditionalJump condition' trueLabel falseLabel
                Assign lhs rhs -> unary Assign lhs rhs
                Case lhs a jumpTable endLabel -> do
                  a' <- currentDefinition' a
                  lhs' <- genName' lhs
                  pure $ lined' $ Case lhs' a' jumpTable endLabel
                TracInternal internal -> pure $ lined' $ TracInternal internal
                Abort lineNumber reason -> do
                  reason' <- case reason of
                    DispatchOnVoid -> pure DispatchOnVoid
                    StaticDispatchOnVoid -> pure StaticDispatchOnVoid
                    CaseOnVoid -> pure CaseOnVoid
                    CaseNoMatch v -> do
                      v' <- currentDefinition' v
                      pure $ CaseNoMatch v'
                    DivisionByZero -> pure DivisionByZero
                    SubstringOutOfRange -> pure SubstringOutOfRange
                  pure $ lined' $ Abort lineNumber reason'
                Phi _ _ -> error "Phi function /should/ be unreachable in rename" -- cant happen
      )
      blockStatements

  -- insert phi functions
  let phiFunctionsBlock = Map.findWithDefault Map.empty block phiFunctions
      phiFunctionBlock' =
        Map.toList $
          Map.mapWithKey
            ( \var labels ->
                Set.fromList $ catMaybes $ Set.toList $ Set.map (\label -> Map.findWithDefault Nothing var (Map.map Just $ Map.findWithDefault Map.empty label openingVars)) labels
            )
            phiFunctionsBlock
  phiFunctionCode <- forM phiFunctionBlock' (\(dst, srcs) -> flip Phi srcs <$> genName dst)
  let blockStatements'' = head blockStatements' : fmap (Lined 0) phiFunctionCode ++ tail blockStatements'
      blocks' = Map.insert block blockStatements'' blocks

  closingVars <- gets (\ours -> Map.insert block ours openingVars)

  foldM (\b label -> rename b domTree phiFunctions closingVars label) blocks' (Set.toList (Map.findWithDefault Set.empty block domTree))

genName' :: SsaVariable -> State (Map.Map Variable SsaVariable) SsaVariable
genName' (SsaVariable v _) = genName v

genName :: Variable -> State (Map.Map Variable SsaVariable) SsaVariable
genName var = do
  SsaVariable v n <- gets $ Map.findWithDefault (SsaVariable var 0) var
  let newVar = SsaVariable v (n + 1)
  modify $ Map.insert var newVar
  pure newVar

currentDefinition' :: SsaVariable -> State (Map.Map Variable SsaVariable) SsaVariable
currentDefinition' (SsaVariable v _) = currentDefinition v

currentDefinition :: Variable -> State (Map.Map Variable SsaVariable) SsaVariable
currentDefinition var = gets $ Map.findWithDefault (SsaVariable var 0) var

-- Cooper, Keith D., Harvey, Timothy J. and Kennedy, Ken. "A simple, fast dominance algorithm."
dominanceFrontiers :: Cfg s v -> Map.Map Label (Set.Set Label)
dominanceFrontiers cfg =
  let idoms = idom cfg
   in Map.foldlWithKey'
        ( \frontiers b bPredecessors ->
            if length bPredecessors >= 2
              then
                foldl'
                  (\frontiers' p -> dominanceFrontiers' idoms p b frontiers')
                  frontiers
                  bPredecessors
              else frontiers
        )
        (Map.fromList $ map (,Set.empty) $ Map.keys $ cfgBlocks cfg) -- we start with each label dominating the empty set
        (cfgPredecessors cfg)

-- from https://en.wikipedia.org/wiki/Static_single-assignment_form
dominanceFrontiers' :: Map.Map Label Label -> Label -> Label -> Map.Map Label (Set.Set Label) -> Map.Map Label (Set.Set Label)
dominanceFrontiers' idoms runner b frontiers =
  if runner == idoms Map.! b
    then frontiers
    else
      dominanceFrontiers'
        idoms
        (idoms Map.! runner)
        b
        ( Map.insert
            runner
            ( Set.insert b $
                Map.findWithDefault Set.empty runner frontiers
            )
            frontiers
        )

-- naive idom algorithm based on the naive dominator algorithm presented at
-- https://en.wikipedia.org/wiki/Dominator_(graph_theory)#Algorithms
--
-- note that the entry node does not have an immediate dominator, so its entry
-- will contain Nothing.
-- idom :: Cfg s v -> Map.Map Label (Maybe Label)
-- idom cfg =
--   let labels = Map.keys $ cfgBlocks cfg

--       dominatorsInitial :: Map.Map Label (Set.Set Label)
--       dominatorsInitial =
--         Map.insert (cfgStart cfg) (Set.singleton $ cfgStart cfg) $
--           Map.fromList $
--             map (,Set.fromList labels) labels

--       dom' :: Map.Map Label (Set.Set Label) -> Map.Map Label (Set.Set Label)
--       dom' doms =
--         foldl'
--           ( \doms' n ->
--               Map.insert
--                 n
--                 (Set.insert n $ Set.unions $ Set.map (doms' Map.!) (cfgPredecessors cfg Map.! n))
--                 doms'
--           )
--           doms
--           (Map.keys doms)

--       fix :: (Eq a) => (a -> a) -> a -> a
--       fix f x
--         | x == f x = x
--         | otherwise = fix f (f x)

--       dominators :: Map.Map Label (Set.Set Label)
--       dominators = fix dom' dominatorsInitial

--       strictDominators :: Map.Map Label (Set.Set Label)
--       strictDominators = Map.mapWithKey Set.delete dominators
--    in Map.mapWithKey
--         ( \n sDom ->
--             Set.lookupMin $
--               Set.filter
--                 (\n' -> not $ Set.member n' $ Set.unions $ Set.toList $ Set.map (strictDominators Map.!) sDom)
--                 sDom
--         )
--         strictDominators

-- Cooper, Keith D., Harvey, Timothy J. and Kennedy, Ken. "A simple, fast dominance algorithm."
-- calculate immediate dominators of all nodes, except for the entry node which gets a Nothing
idom :: Cfg s v -> Map.Map Label Label
idom (Cfg {cfgStart, cfgPredecessors, cfgChildren}) =
  let ordering = reversePostOrder cfgChildren cfgStart
      orderingMap = Map.fromList $ zip ordering [0 ..]
      labels = Map.keys cfgChildren
      startIdoms = Map.singleton cfgStart cfgStart
   in idom' cfgStart cfgPredecessors ordering orderingMap True startIdoms

idom' ::
  Label ->
  Map.Map Label (Set.Set Label) ->
  [Label] ->
  Map.Map Label Int ->
  Bool ->
  Map.Map Label Label ->
  Map.Map Label Label
idom' cfgStart predecessors nodes nodeOrdering changed idoms
  | not changed = idoms
  | otherwise =
      let (idoms', changed') =
            foldl'
              ( \(idoms'', changed'') b ->
                  let preds = predecessors Map.! b
                      firstPred = minimumBy (\a b -> compareInt (nodeOrdering Map.! a) (nodeOrdering Map.! b)) preds
                      newIdom =
                        foldl'
                          ( \newIdom' p ->
                              if isJust $ Map.lookup p idoms''
                                then intersect idoms'' nodeOrdering p newIdom'
                                else newIdom'
                          )
                          firstPred
                          (Set.filter (/= firstPred) preds)
                   in if Map.lookup b idoms'' == Just newIdom
                        then (idoms'', changed'')
                        else (Map.insert b newIdom idoms'', True)
              )
              (idoms, False)
              (tail nodes) -- skip start node
       in idom' cfgStart predecessors nodes nodeOrdering changed' idoms'

intersect :: Map.Map Label Label -> Map.Map Label Int -> Label -> Label -> Label
intersect idoms ordering f1 f2
  | f1 == f2 = f1
  | ordering Map.! f1 > ordering Map.! f2 = intersect idoms ordering (idoms Map.! f1) f2
  | otherwise = intersect idoms ordering f1 (idoms Map.! f2)

-- depth first ordering
dfs :: Map.Map Label (Set.Set Label) -> Label -> [Label]
dfs edges start = dfs' edges [start] Set.empty

dfs' :: Map.Map Label (Set.Set Label) -> [Label] -> Set.Set Label -> [Label]
dfs' edges (current : rest) visited =
  let new = Set.difference (edges Map.! current) visited
      new' = Set.toList new
      visited' = visited <> new
   in current
        : Set.toList new
        ++ dfs' edges (rest ++ new') visited'
dfs' _ [] _ = []

-- reverse post order traversal; see
-- https://eli.thegreenplace.net/2015/directed-graph-traversal-orderings-and-applications-to-data-flow-analysis/
reversePostOrder :: Map.Map Label (Set.Set Label) -> Label -> [Label]
reversePostOrder graph start =
  let dfsWalk :: Label -> State (Set.Set Label, [Label]) ()
      dfsWalk node = do
        modify $ first (Set.insert node)
        let successors = graph Map.! node
        visited <- gets fst
        traverse_ dfsWalk $ Set.filter (not . (`Set.member` visited)) successors
        modify $ second (node :)
   in snd $ execState (dfsWalk start) (Set.empty, [])

-- put here to avoid circular dependencies :)
type AiState a = State (Map.Map SsaVariable a, Set.Set SsaVariable)

-- only actually put a vairable onto the worklist if it changes
aiSet :: (Eq a) => SsaVariable -> a -> AiState a ()
aiSet var value = do
  (state, affected) <- get
  unless (state Map.! var == value) $ modify $ bimap (Map.insert var value) (Set.insert var)

aiLookup :: SsaVariable -> AiState a a
aiLookup var = gets ((Map.! var) . fst)
