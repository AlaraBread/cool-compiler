-- We do dead code elimination. It explains what it is in the name. What else do
-- you want from me.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DeadCodeElimination (deadCodeElimination) where

import Cfg
import Control.Monad.State
import qualified Data.List as List
import Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Ssa
import TracIr
import Util

data Liveness = Live | Dead
  deriving (Eq)

instance Lattice Liveness where
  top = Live
  bottom = Dead
  meet a b
    | a == b = a
    | otherwise = bottom
  join a b
    | a == b = a
    | otherwise = top

type LaState = AiState Liveness

kill :: SsaVariable -> LaState ()
kill var = aiSet var Dead

vivify :: SsaVariable -> LaState ()
vivify var = aiSet var Live

vivifyAttributes :: LaState ()
vivifyAttributes =
  let isAttribute :: SsaVariable -> Bool
      isAttribute (SsaVariable (AttributeV v) _) = True
      isAttribute (SsaVariable _ _) = False
   in do
        variables <- gets (Map.keys . fst)
        let attributes = Prelude.filter isAttribute variables
        mapM_ vivify attributes

unary :: SsaVariable -> SsaVariable -> LaState ()
unary dst src = kill dst *> vivify src

binary :: SsaVariable -> SsaVariable -> SsaVariable -> LaState ()
binary dst src1 src2 = kill dst *> vivify src1 *> vivify src2

-- note that this is a backwards analysis, i.e. we are updating the estimates
-- from /after/ statement runs to those for /before/ statement runs
transferFunction' :: TracStatement SsaVariable -> LaState ()
transferFunction' statement = case statement of
  Add dst src1 src2 -> binary dst src1 src2
  Subtract dst src1 src2 -> binary dst src1 src2
  Multiply dst src1 src2 -> binary dst src1 src2
  Divide dst src1 src2 -> binary dst src1 src2
  LessThan dst src1 src2 -> binary dst src1 src2
  LessThanOrEqualTo dst src1 src2 -> binary dst src1 src2
  Equals dst src1 src2 -> binary dst src1 src2
  IntConstant dst _ -> kill dst
  BoolConstant dst _ -> kill dst
  StringConstant dst _ -> kill dst
  Not dst src -> unary dst src
  Negate dst src -> unary dst src
  New dst _ -> kill dst
  Default dst _ -> kill dst
  IsVoid dst src -> unary dst src
  -- we need to vivify all attributes, because we do not know if the function
  -- call is on self. we /could/ do some heuristic about static calls or
  -- receiver types in here to have to do this less often.
  Dispatch {dispatchResult = dst, dispatchReceiver = receiver, dispatchArgs = args} ->
    kill dst *> vivify receiver *> mapM_ vivify args *> vivifyAttributes
  Jump _ -> pure ()
  TracLabel _ -> pure ()
  -- we need to vivify attributes here as well, as other methods can access them. th
  Return src -> vivify src *> vivifyAttributes
  Comment _ -> pure ()
  ConditionalJump src _ _ -> vivify src
  Assign dst src -> unary dst src
  Case dst src _ _ -> unary dst src
  TracInternal _ -> pure ()
  Abort _ _ -> pure ()
  Phi dst srcs -> kill dst *> mapM_ vivify srcs

instance Ai (TracStatement SsaVariable) SsaVariable Liveness where
  transferFunction estimates statement =
    execState (transferFunction' statement) (estimates, Set.empty)

-- tells us if a statement is dead. statements that can cause side effects
-- outside of variable assignment (function calls, aborts) are never
-- considered dead.
isStatementDead :: (TracStatement SsaVariable, Map.Map SsaVariable (Liveness, Liveness)) -> Bool
isStatementDead (statement, estimateMap) =
  let isDeadAfter var = snd (estimateMap Map.! var) == Dead
   in case statement of
        Add dst src1 src2 -> isDeadAfter dst
        Subtract dst src1 src2 -> isDeadAfter dst
        Multiply dst src1 src2 -> isDeadAfter dst
        -- it is safe to consider this non-side affecting because we guard
        -- division by zero with a conditional
        Divide dst src1 src2 -> isDeadAfter dst
        LessThan dst src1 src2 -> isDeadAfter dst
        LessThanOrEqualTo dst src1 src2 -> isDeadAfter dst
        Equals dst src1 src2 -> isDeadAfter dst
        IntConstant dst _ -> isDeadAfter dst
        BoolConstant dst _ -> isDeadAfter dst
        StringConstant dst _ -> isDeadAfter dst
        Not dst src -> isDeadAfter dst
        Negate dst src -> isDeadAfter dst
        New dst _ -> isDeadAfter dst
        Default dst _ -> isDeadAfter dst
        IsVoid dst src -> isDeadAfter dst
        -- we need to vivify all attributes, because we do not know if the function
        -- call is on self. we /could/ do some heuristic about static calls or
        -- receiver types in here to have to do this less often.
        Dispatch {} -> False
        Jump _ -> False
        TracLabel _ -> False
        Return src -> False
        Comment _ -> False
        ConditionalJump src _ _ -> False
        Assign dst src -> isDeadAfter dst
        -- note that case can jump to side-effecting code, even if we do not use the result
        Case dst src _ _ -> False
        TracInternal _ -> False
        Abort _ _ -> False
        Phi dst srcs -> False

-- We combine two nodes in the CFG into one basic block.
--
-- Preconditions: parent has exactly one child (child), and child has exactly
-- one parent (parent). (i.e. it is valid to combine them into one basic block)
combineBlocks ::
  Cfg (TracStatement SsaVariable) SsaVariable ->
  Label ->
  Label ->
  Cfg (TracStatement SsaVariable) SsaVariable
combineBlocks cfg parent child =
  let parentCode = cfgBlocks cfg Map.! parent
      childCode = cfgBlocks cfg Map.! child
      cfgBlocks' = Map.insert parent (parentCode ++ childCode) $ Map.delete child $ cfgBlocks cfg

      -- note that the parent had exactly one child, child. Therefore, we can
      -- simply replace its children with child's children and everything is
      -- fine.
      childChildren = cfgChildren cfg Map.! child
      cfgChildren' = Map.insert parent childChildren $ cfgChildren cfg

      cfgPredecessors' = Map.delete child $ cfgPredecessors cfg

      parentVariables = cfgVariables cfg Map.! parent
      childVariables = cfgVariables cfg Map.! child
      cfgVariables' = Map.insert parent (Set.union parentVariables childVariables) $ Map.delete child $ cfgVariables cfg
   in cfg
        { cfgBlocks = cfgBlocks',
          cfgChildren = cfgChildren',
          cfgPredecessors = cfgPredecessors',
          cfgVariables = cfgVariables'
        }

-- combine all blocks where the control flow is unambiguous.
combineUnambiguousBlocks :: Cfg (TracStatement SsaVariable) SsaVariable -> Cfg (TracStatement SsaVariable) SsaVariable
combineUnambiguousBlocks cfg =
  let cfgChildrenSingular :: Map.Map Label Label
      cfgChildrenSingular = Map.map (fromJust . Set.lookupMin) $ Map.filter ((1 ==) . Set.size) $ cfgChildren cfg

      candidates :: Map Label Label
      candidates = Map.filter (\child -> 1 == Set.size (cfgPredecessors cfg Map.! child)) cfgChildrenSingular
   in -- we do these transformations all at the same time because our
      -- transformations need to all occur in the same order. consider a -> b ->
      -- c; if we do some steps of combining a and b, then some steps of
      -- combining b and c, then some other steps of combining a and b; this
      -- would be bad.
      Map.foldlWithKey' combineBlocks cfg candidates

-- remove unreachable blocks from the CFG.
removeUnreachableBlocks :: Cfg (TracStatement SsaVariable) SsaVariable -> Cfg (TracStatement SsaVariable) SsaVariable
removeUnreachableBlocks cfg =
  let unreachable label = (label /= cfgStart cfg) && Set.null (cfgPredecessors cfg Map.! label)
      unreachableBlocks = Prelude.filter unreachable $ Map.keys $ cfgBlocks cfg

      -- we do not need to alter predecessors because by definition we only remove blocks without predecessors
      cfgBlocks' = List.foldl' (flip Map.delete) (cfgBlocks cfg) unreachableBlocks
      cfgChildren' = List.foldl' (flip Map.delete) (cfgChildren cfg) unreachableBlocks
      cfgVariables' = List.foldl' (flip Map.delete) (cfgVariables cfg) unreachableBlocks
   in cfg
        { cfgBlocks = cfgBlocks',
          cfgChildren = cfgChildren',
          cfgVariables = cfgVariables'
        }

deadCodeElimination :: Cfg (TracStatement SsaVariable) SsaVariable -> Cfg (TracStatement SsaVariable) SsaVariable
deadCodeElimination cfg =
  let annotatedCfg :: AnnotatedCfg (TracStatement SsaVariable) SsaVariable Liveness
      annotatedCfg = reverseCfg $ runAi $ reverseCfg cfg
      annotatedCfg' = annotatedCfg {cfgBlocks = Map.map (Prelude.filter $ not . isStatementDead . item) $ cfgBlocks annotatedCfg}
   in removeUnreachableBlocks $ combineUnambiguousBlocks $ removeAnnotations annotatedCfg'
