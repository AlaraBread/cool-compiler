{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Ssa where

import Cfg (Cfg (..))
import Control.Monad (foldM, unless)
import Control.Monad.State
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (Foldable (foldl'), find)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Set as Set
import Debug.Trace (trace, traceM, traceShowId)
import TracIr (AbortReason (..), TracStatement (..))
import Util (Label, Lined (Lined), Variable, reverseMap)

data SsaVariable = SsaVariable Variable Int
  deriving (Ord, Eq)

instance Show SsaVariable where
  show (SsaVariable v i) = show v ++ "_" ++ show i

generateSsa :: Cfg (TracStatement Variable) Variable -> Cfg (TracStatement SsaVariable) SsaVariable
generateSsa cfg =
  let domFrontiers = dominanceFrontiers cfg
      Cfg {cfgStart, cfgBlocks, cfgChildren, cfgPredecessors, cfgDefinitions} = cfg
      domTree = Map.insert cfgStart Set.empty (reverseMap $ Map.map Set.singleton $ idom cfg)
      revVariableDefinitions = reverseMap cfgDefinitions
      phiFunctions =
        Map.map
          ( Map.fromList
              . map
                (\v -> (SsaVariable v 0, Set.singleton $ SsaVariable v 0))
              . Set.toList
          )
          $ calculatePhiFunctions domFrontiers revVariableDefinitions
      (_, cfgBlocks') =
        evalState
          ( rename
              (fillVariablesWithZero cfgBlocks)
              domTree
              cfgChildren
              phiFunctions
              cfgStart
          )
          (Map.empty, Map.empty)
   in Cfg cfgStart cfgBlocks' cfgChildren cfgPredecessors Map.empty Map.empty

calculatePhiFunctions ::
  Map.Map Label (Set.Set Label) ->
  Map.Map Variable (Set.Set Label) ->
  Map.Map Label (Set.Set Variable)
calculatePhiFunctions domFrontiers =
  Map.foldlWithKey'
    ( \p variable blocks ->
        foldl'
          ( \p' block ->
              foldl'
                ( \p'' k ->
                    Map.insert
                      k
                      (Set.insert variable $ Map.findWithDefault Set.empty block p'')
                      p''
                )
                p'
                (domFrontiers Map.! block)
          )
          p
          blocks
    )
    Map.empty

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
  Map.Map Label (Set.Set Label) ->
  Map.Map Label (Map.Map SsaVariable (Set.Set SsaVariable)) ->
  Label ->
  State
    (Map.Map Variable SsaVariable, Map.Map Variable Int)
    ( Map.Map
        Label
        ( Map.Map SsaVariable (Set.Set SsaVariable)
        ),
      Map.Map Label [Lined (TracStatement SsaVariable)]
    )
rename blocks domTree successors phiFunctions block =
  do
    (oldDefinitions, _) <- get
    let phiFunctionsForBlock = Map.findWithDefault Map.empty block phiFunctions
    phiFunctionsForBlock' <-
      traverse
        (\(lhs, rhs) -> (,rhs) <$> genName' lhs)
        (Map.toList phiFunctionsForBlock)
    let phiFunctionsForBlock'' = Map.fromList phiFunctionsForBlock'
        phiFunctions' = Map.insert block phiFunctionsForBlock'' phiFunctions
    blockStatements <-
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
                  Phi _ _ -> undefined -- cant happen
        )
        (blocks Map.! block)
    let blocks' = Map.insert block blockStatements blocks
    phiFunctions'' <-
      foldM
        ( \phiFunctions'' successor -> do
            let phiFunctionsForSuccessor = Map.findWithDefault Map.empty successor phiFunctions''
            phiFunctionsForSuccessor' <-
              (Set.fromList <$>)
                <$> traverse
                  (traverse currentDefinition' . Set.toList)
                  phiFunctionsForSuccessor
            pure $ Map.insert successor phiFunctionsForSuccessor' phiFunctions''
        )
        phiFunctions'
        (successors Map.! block)
    (phiFunctions''', blocks'') <-
      foldM
        (\(p, b) -> rename b domTree successors p)
        (phiFunctions'', blocks')
        (domTree Map.! block)
    (_, counters) <- get
    put (oldDefinitions, counters)
    pure (phiFunctions''', blocks'')

genName' :: SsaVariable -> State (Map.Map Variable SsaVariable, Map.Map Variable Int) SsaVariable
genName' (SsaVariable v _) = genName v

genName :: Variable -> State (Map.Map Variable SsaVariable, Map.Map Variable Int) SsaVariable
genName v = do
  (currentDefinitions, counters) <- get
  let c = fromMaybe 0 $ Map.lookup v counters
  put (currentDefinitions, Map.insert v (c + 1) counters)
  pure $ SsaVariable v c

currentDefinition' :: SsaVariable -> State (Map.Map Variable SsaVariable, Map.Map Variable Int) SsaVariable
currentDefinition' (SsaVariable v _) = currentDefinition v

currentDefinition :: Variable -> State (Map.Map Variable SsaVariable, Map.Map Variable Int) SsaVariable
currentDefinition v = do
  (currentDefinitions, _) <- get
  pure $ fromMaybe (SsaVariable v 0) $ Map.lookup v currentDefinitions

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
  if runner == fromJust (Map.lookup b idoms)
    then frontiers
    else
      dominanceFrontiers'
        idoms
        (idoms Map.! runner)
        b
        ( Map.insert
            runner
            ( Set.insert b $
                fromMaybe Set.empty $
                  Map.lookup runner frontiers
            )
            frontiers
        )

-- Cooper, Keith D., Harvey, Timothy J. and Kennedy, Ken. "A simple, fast dominance algorithm."
-- calculate immidiate dominators of all nodes
idom :: Cfg s v -> Map.Map Label Label
idom (Cfg {cfgStart, cfgPredecessors}) =
  let ordering = dfs cfgPredecessors cfgStart
      ordering' = case ordering of
        (_ : o) -> o
        [] -> []
      ordering'' = reverse ordering'
      orderingMap = Map.fromList [(n, i) | n <- ordering'', i <- [0 :: Int ..]]
   in idom' cfgPredecessors ordering'' orderingMap True (Map.singleton cfgStart cfgStart)

idom' ::
  Map.Map Label (Set.Set Label) ->
  [Label] ->
  Map.Map Label Int ->
  Bool ->
  Map.Map Label Label ->
  Map.Map Label Label
idom' predecessors nodes nodeOrdering changed idoms
  | not changed = idoms
  | otherwise =
      let (idoms', changed') =
            foldl'
              ( \(idoms'', changed'') b ->
                  let preds = predecessors Map.! b
                      firstPred = fromJust $ find (\i -> isJust $ Map.lookup i idoms'') preds
                      newIdom =
                        foldl'
                          ( \newIdom' p ->
                              if isJust $ Map.lookup p idoms''
                                then intersect idoms'' nodeOrdering p newIdom'
                                else newIdom'
                          )
                          firstPred
                          preds
                   in if Map.lookup b idoms'' == Just newIdom
                        then (idoms'', changed'')
                        else (Map.insert b newIdom idoms'', True)
              )
              (idoms, False)
              nodes
       in idom' predecessors nodes nodeOrdering changed' idoms'

intersect :: Map.Map Label Label -> Map.Map Label Int -> Label -> Label -> Label
intersect idoms ordering f1 f2
  | ordering Map.! f1 == ordering Map.! f2 = f1
  | ordering Map.! f1 < ordering Map.! f2 = intersect idoms ordering (idoms Map.! f1) f2
  | otherwise = intersect idoms ordering f1 (idoms Map.! f2)

-- depth first ordering
dfs :: Map.Map Label (Set.Set Label) -> Label -> [Label]
dfs edges start = dfs' edges [start] Set.empty

dfs' :: Map.Map Label (Set.Set Label) -> [Label] -> Set.Set Label -> [Label]
dfs' edges (current : rest) visited =
  let new = Set.difference (fromJust $ Map.lookup current edges) visited
      new' = Set.toList new
      visited' = visited <> new
   in current
        : Set.toList new
        ++ dfs' edges (rest ++ new') visited'
dfs' _ [] _ = []

-- put here to avoid circular dependencies :)
type AiState a = State (Map.Map SsaVariable a, Set.Set SsaVariable)

-- only actually put a vairable onto the worklist if it changes
aiSet :: (Eq a) => SsaVariable -> a -> AiState a ()
aiSet var value = do
  (state, affected) <- get
  unless (state Map.! var == value) $ modify $ bimap (Map.insert var value) (Set.insert var)

aiLookup :: SsaVariable -> AiState a a
aiLookup var = gets ((Map.! var) . fst)
