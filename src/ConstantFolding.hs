{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ConstantFolding (constantFold) where

-- We do constant folding here.

import Cfg
import Control.Monad (foldM, unless)
import Control.Monad.State
import Data.Int (Int32)
import qualified Data.List as List
import Data.Map.Strict as Map
import Data.Maybe as Maybe
import qualified Data.Set as Set
import Ssa
import TracIr
import Util

-- a lattice for constant folding. we have a top, all constants at the same
-- level, then a bottom. We include both int and bool because this allows us to
-- appropriately deal with things like x <- 1 == 1. We also do void/nonvoid to
-- let us potentially elide void checks on dispatch. We also keep track of
-- strings because why not.
data Constant
  = ConstantTop
  | ConstantInt !Int32
  | ConstantBool !Bool
  | ConstantString !String
  | ConstantVoid
  | ConstantNonVoid
  | ConstantBottom
  deriving (Eq)

instance Lattice Constant where
  top = ConstantTop
  bottom = ConstantBottom
  meet a b
    | a == b = a
    | a == ConstantBottom = ConstantBottom
    | b == ConstantBottom = ConstantBottom
    | a == ConstantTop = b
    | b == ConstantTop = a
  join a b
    | a == b = a
    | a == ConstantBottom = b
    | b == ConstantBottom = a
    | a == ConstantTop = ConstantTop
    | b == ConstantTop = ConstantTop

type CFState = AiState Constant

-- there is probably a nice way to abstract this, but it is also probably more trouble than it is worth :)
binaryInt :: (Int32 -> Int32 -> Int32) -> SsaVariable -> SsaVariable -> SsaVariable -> CFState ()
binaryInt op dst src1 src2 = do
  src1' <- aiLookup src1
  src2' <- aiLookup src2

  case src1' of
    ConstantInt src1'' -> case src2' of
      ConstantInt src2'' -> aiSet dst $ ConstantInt $ op src1'' src2''
      _ -> pure ()
    _ -> pure ()

compareInt :: (Int32 -> Int32 -> Bool) -> SsaVariable -> SsaVariable -> SsaVariable -> CFState ()
compareInt op dst src1 src2 = do
  src1' <- aiLookup src1
  src2' <- aiLookup src2

  case src1' of
    ConstantInt src1'' -> case src2' of
      ConstantInt src2'' -> aiSet dst $ ConstantBool $ op src1'' src2''
      _ -> pure ()
    _ -> pure ()

topifyAttributes :: CFState ()
topifyAttributes =
  let isAttribute :: SsaVariable -> Bool
      isAttribute (SsaVariable (AttributeV v) _) = True
      isAttribute (SsaVariable _ _) = False
   in do
        variables <- gets (Map.keys . fst)
        let attributes = Prelude.filter isAttribute variables
        mapM_ (`aiSet` ConstantTop) attributes

-- we need to be careful to avoid division by zero. Other than that, since
-- constants are already Int32s, we should be okay to just do Haskell
-- arithmetic.
transferFunction' :: TracStatement SsaVariable -> CFState ()
transferFunction' statement = case statement of
  Add dst src1 src2 -> binaryInt (+) dst src1 src2
  Subtract dst src1 src2 -> binaryInt (-) dst src1 src2
  Multiply dst src1 src2 -> binaryInt (*) dst src1 src2
  Divide dst src1 src2 -> do
    src1' <- aiLookup src1
    src2' <- aiLookup src2

    case src1' of
      ConstantInt src1'' -> case src2' of
        ConstantInt src2'' -> unless (src2'' == 0) $ aiSet dst $ ConstantInt $ src1'' `div` src2''
        _ -> pure ()
      _ -> pure ()
  -- TODO: you can compare other things...
  LessThan dst src1 src2 -> compareInt (<) dst src1 src2
  LessThanOrEqualTo dst src1 src2 -> compareInt (Prelude.<=) dst src1 src2
  Equals dst src1 src2 -> compareInt (==) dst src1 src2
  IntConstant dst value -> aiSet dst $ ConstantInt value
  BoolConstant dst value -> aiSet dst $ ConstantBool value
  StringConstant dst value -> aiSet dst $ ConstantString value
  Not dst src -> do
    src' <- aiLookup src
    case src' of
      ConstantBool bool -> aiSet dst $ ConstantBool $ not bool
      _ -> pure ()
  Negate dst src -> do
    src' <- aiLookup src
    case src' of
      ConstantInt val -> aiSet dst $ ConstantInt $ -val
      _ -> pure ()
  New dst type' -> case type' of
    Type "String" -> aiSet dst $ ConstantString ""
    Type "Int" -> aiSet dst $ ConstantInt 0
    Type "Bool" -> aiSet dst $ ConstantBool False
    Type _ -> aiSet dst ConstantNonVoid
  Default dst type' -> case type' of
    Type "String" -> aiSet dst $ ConstantString ""
    Type "Int" -> aiSet dst $ ConstantInt 0
    Type "Bool" -> aiSet dst $ ConstantBool False
    Type _ -> aiSet dst ConstantVoid
  IsVoid dst src -> do
    src' <- aiLookup src
    case src' of
      ConstantVoid -> aiSet dst $ ConstantBool True
      ConstantNonVoid -> aiSet dst $ ConstantBool False
      ConstantInt _ -> aiSet dst $ ConstantBool False
      ConstantBool _ -> aiSet dst $ ConstantBool False
      ConstantString _ -> aiSet dst $ ConstantBool False
      _ -> pure ()
  -- we need to remove any constant values we had for all attributes (i.e. go to
  -- top), because we do not know if the function call is on self. we /could/ do
  -- some heuristic about static calls or receiver types in here to have to do
  -- this less often.
  Dispatch {} -> topifyAttributes
  Jump _ -> pure ()
  TracLabel _ -> pure ()
  Return src -> pure ()
  Comment _ -> pure ()
  ConditionalJump {} -> pure ()
  Assign dst src -> aiLookup src >>= aiSet dst
  Case dst src _ _ -> pure ()
  TracInternal _ -> pure ()
  Abort _ _ -> pure ()
  Phi dst srcs -> do
    srcValues <- traverse aiLookup $ Set.toList srcs
    let val = List.foldl' Cfg.join ConstantBottom srcValues
    aiSet dst val

instance Ai (TracStatement SsaVariable) SsaVariable Constant where
  transferFunction estimates statement =
    execState (transferFunction' statement) (estimates, Set.empty)

-- we insert constant setting wherever the state changes before/after execution;
-- DCE should eliminate the statement if possible.
insertConstantSettingStatement ::
  (TracStatement SsaVariable, Map SsaVariable (Constant, Constant)) ->
  [(TracStatement SsaVariable, Map SsaVariable (Constant, Constant))]
insertConstantSettingStatement statement =
  let changedVariables = Map.filter (uncurry (/=)) $ snd statement
      toSet :: [(SsaVariable, Constant)]
      toSet = Map.toList $ Map.map snd changedVariables

      -- empty maps are okay here because we destroy constant folding information after this anyways
      constantSetting :: SsaVariable -> Constant -> Maybe (TracStatement SsaVariable, Map v (a, a))
      constantSetting dst after = case after of
        ConstantTop -> Nothing
        ConstantInt val -> Just (IntConstant dst val, Map.empty)
        ConstantBool val -> Just (BoolConstant dst val, Map.empty)
        ConstantString val -> Just (StringConstant dst val, Map.empty)
        -- No type named internal exists, but we need something that we
        -- /definitely/ know will be boxed, as Default for any unboxed type
        -- produces a value of exactly void. This is definitely hacky :).
        ConstantVoid -> Just (Default dst $ Type "internal", Map.empty)
        ConstantNonVoid -> Nothing
        ConstantBottom -> Nothing

      constantSettings = Maybe.mapMaybe (uncurry constantSetting) toSet
   in statement : constantSettings

insertConstantSetting :: AnnotatedCfg (TracStatement SsaVariable) SsaVariable Constant -> AnnotatedCfg (TracStatement SsaVariable) SsaVariable Constant
insertConstantSetting cfg = cfg {cfgBlocks = Map.map (concatMap (mapM insertConstantSettingStatement)) $ cfgBlocks cfg}

-- If the statement provided is a conditional jump that can be made
-- unconditional, do so, and return the outgoing edge to be removed from the
-- CFG. Otherwise, this function is the identity in the first return value and
-- returns Nothing in the second.
rewriteJump ::
  Lined (TracStatement SsaVariable, Map SsaVariable (Constant, Constant)) ->
  (Lined (TracStatement SsaVariable, Map SsaVariable (Constant, Constant)), Maybe Label)
rewriteJump (Lined line (statement, estimates)) =
  case statement of
    ConditionalJump var trueBranch falseBranch -> case fst $ estimates Map.! var of
      ConstantBool True -> (Lined line (Jump trueBranch, estimates), Just falseBranch)
      ConstantBool False -> (Lined line (Jump falseBranch, estimates), Just trueBranch)
      _ -> (Lined line (statement, estimates), Nothing)
    _ -> (Lined line (statement, estimates), Nothing)

-- rewrite conditional jumps on constant booleans to unconditional jumps, and
-- make the relevant changes to the cfg
rewriteJumps ::
  AnnotatedCfg (TracStatement SsaVariable) SsaVariable Constant ->
  AnnotatedCfg (TracStatement SsaVariable) SsaVariable Constant
rewriteJumps cfg =
  let rewriteJumpsInBlock ::
        [Lined (TracStatement SsaVariable, Map SsaVariable (Constant, Constant))] ->
        ([Lined (TracStatement SsaVariable, Map SsaVariable (Constant, Constant))], [Maybe Label])
      rewriteJumpsInBlock = unzip . fmap rewriteJump

      rewritten = Map.map rewriteJumpsInBlock (cfgBlocks cfg)
      cfgBlocks' = Map.map fst rewritten

      extraneousEdges :: Map.Map Label (Set.Set Label)
      extraneousEdges = Map.map (Set.fromList . catMaybes . snd) rewritten
      extraneousEdgesReversed = reverseMap extraneousEdges

      cfgChildren' =
        Map.mapWithKey
          (\parent children -> Set.difference children $ extraneousEdges Map.! parent)
          $ cfgChildren cfg
      cfgPredecessors' =
        Map.mapWithKey
          (\child parents -> Set.difference parents $ extraneousEdgesReversed Map.! child)
          $ cfgPredecessors cfg
   in cfg
        { cfgBlocks = cfgBlocks',
          cfgChildren = cfgChildren',
          cfgPredecessors = cfgPredecessors'
        }

constantFold :: Cfg (TracStatement SsaVariable) SsaVariable -> Cfg (TracStatement SsaVariable) SsaVariable
constantFold cfg =
  let annotatedCfg :: AnnotatedCfg (TracStatement SsaVariable) SsaVariable Constant
      annotatedCfg = runAi cfg
      annotatedCfg' = insertConstantSetting $ rewriteJumps annotatedCfg
   in removeAnnotations annotatedCfg'
