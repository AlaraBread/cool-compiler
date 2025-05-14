-- We do dead code elimination. It explains what it is in the name. What else do
-- you want from me.

module DeadCodeElimination where

import Cfg
import Control.Monad (unless)
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.Map.Strict as Map
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

type LAState = State (Map.Map SsaVariable Liveness, Set.Set SsaVariable)

-- only actually put a vairable onto the worklist if it changes
set :: Liveness -> SsaVariable -> LAState ()
set liveness var = do
  (state, affected) <- get
  unless (state Map.! var == liveness) $ modify $ bimap (Map.insert var Dead) (Set.insert var)

kill :: SsaVariable -> LAState ()
kill = set Dead

vivify :: SsaVariable -> LAState ()
vivify = set Live

vivifyAttributes :: LAState ()
vivifyAttributes =
  let isAttribute :: SsaVariable -> Bool
      isAttribute (SsaVariable (AttributeV v) _) = True
      isAttribute (SsaVariable _ _) = False
   in do
        variables <- gets (Map.keys . fst)
        let attributes = Prelude.filter isAttribute variables
        mapM_ vivify attributes

unary :: SsaVariable -> SsaVariable -> LAState ()
unary dst src = kill dst *> vivify src

binary :: SsaVariable -> SsaVariable -> SsaVariable -> LAState ()
binary dst src1 src2 = kill dst *> vivify src1 *> vivify src2

-- note that this is a backwards analysis, i.e. we are updating the estimates
-- from /after/ statement runs to those for /before/ statement runs
transferFunction' :: TracStatement SsaVariable -> LAState ()
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

deadCodeElimination :: Cfg (TracStatement SsaVariable) SsaVariable -> Cfg (TracStatement SsaVariable) SsaVariable
deadCodeElimination cfg =
  let annotatedCfg :: AnnotatedCfg (TracStatement SsaVariable) SsaVariable Liveness
      annotatedCfg = reverseCfg $ runAi $ reverseCfg cfg
      annotatedCfg' = annotatedCfg {cfgBlocks = Map.map (Prelude.filter $ not . isStatementDead . item) $ cfgBlocks annotatedCfg}
   in removeAnnotations annotatedCfg'
