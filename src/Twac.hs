{-# LANGUAGE NamedFieldPuns #-}

module Twac where

import Control.Monad.State
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import InputIr (Formal)
import qualified InputIr
import qualified Trac
import qualified TracIr
import Util

type Twac v = [Lined (TwacStatement v)]

-- Note: we follow src, dst ordering here. This matches AT&T syntax in general.
-- This is mildly backwards from Trac, where we have dest src1 src2.

-- We also turn case statements into jump tables in the process.
data TwacStatement v
  = Add v v
  | Subtract v v
  | Multiply v v
  | Divide v v
  | LessThan v v v
  | LessThanOrEqualTo v v v
  | Equals v v v
  | IntConstant Int32 v
  | BoolConstant Bool v
  | StringConstant String v
  | Not v
  | Negate v
  | New Type v
  | Default Type v
  | IsVoid v
  | Dispatch
      { dispatchResult :: v,
        dispatchReceiver :: v,
        dispatchReceiverType :: Type,
        dispatchType :: Maybe Type,
        dispatchMethod :: String,
        dispatchArgs :: [v]
      }
  | Jump Label
  | TwacLabel Label
  | Return v
  | Comment String
  | ConditionalJump v Label
  | Assign v v
  | Copy v v
  | TwacCase v (Map.Map Type Label)
  | Abort Int (TracIr.AbortReason v)
  | TwacInternal InputIr.Internal

data TwacIr v = TwacIr
  { implementationMap :: Map.Map Type [InputIr.ImplementationMapEntry (TwacMethod v)],
    typeDetails :: TypeDetailsMap
  }

data TwacMethod v = TwacMethod {methodName :: String, body :: Twac v, formals :: [Formal], temporaryCount :: Int}

instance (Show v) => Show (TwacStatement v) where
  show statement =
    let showUnary op dst = show dst ++ " <- " ++ op ++ show dst
        showBinary src dst op = show dst ++ " <- " ++ show dst ++ " " ++ op ++ " " ++ show src
        showImmediate immediate dst = show dst ++ " <- " ++ show immediate
     in case statement of
          Add src dst -> showBinary src dst "+"
          Subtract src dst -> showBinary src dst "-"
          Multiply src dst -> showBinary src dst "*"
          Divide src dst -> showBinary src dst "/"
          LessThan src1 src2 dst -> show dst ++ " <- " ++ show src1 ++ " < " ++ show src2
          LessThanOrEqualTo src1 src2 dst -> show dst ++ " <- " ++ show src1 ++ " <= " ++ show src2
          Equals src1 src2 dst -> show dst ++ " <- " ++ show src1 ++ " <= " ++ show src2
          IntConstant i dst -> showImmediate i dst
          BoolConstant i dst -> showImmediate i dst
          StringConstant i dst -> showImmediate i dst
          Not dst -> showUnary "not " dst
          Negate dst -> showUnary "~" dst
          New (Type t) dst -> show dst ++ " <- new " ++ t
          Default (Type t) dst -> show dst ++ " <- default " ++ t
          IsVoid dst -> showUnary "isVoid " dst
          -- TODO: this is incomplete
          Dispatch {dispatchMethod, dispatchReceiver} -> show dispatchReceiver ++ "." ++ dispatchMethod
          Jump lbl -> "jmp " ++ show lbl
          TwacLabel lbl -> "label " ++ show lbl
          Return var -> "return " ++ show var
          Comment str -> "comment " ++ str
          ConditionalJump var lbl -> "bt " ++ show var ++ " " ++ show lbl
          Assign src dst -> show dst ++ " <- " ++ show src
          Copy src dst -> "copy " ++ show src ++ " " ++ show dst
          -- TODO: this is incomplete
          TwacCase var _ -> "case " ++ show var
          Abort line reason -> "abort " ++ show line ++ ": " ++ show reason
          TwacInternal internal -> "internal: " ++ show internal

generateUnaryStatement :: (v -> TwacStatement v) -> v -> v -> [TwacStatement v]
generateUnaryStatement op dst src =
  [Copy src dst, op dst]

generateBinaryStatement :: (v -> v -> TwacStatement v) -> v -> v -> v -> [TwacStatement v]
generateBinaryStatement op dst src1 src2 =
  [Copy src1 dst, op src2 dst]

generateTwacStatement :: TracIr.TracStatement v -> State Temporary [TwacStatement v]
generateTwacStatement tracStatement = case tracStatement of
  TracIr.Add dst src1 src2 -> pure $ generateBinaryStatement Add dst src1 src2
  TracIr.Subtract dst src1 src2 -> pure $ generateBinaryStatement Subtract dst src1 src2
  TracIr.Multiply dst src1 src2 -> pure $ generateBinaryStatement Multiply dst src1 src2
  TracIr.Divide dst src1 src2 -> pure $ generateBinaryStatement Divide dst src1 src2
  TracIr.LessThan dst src1 src2 -> pure [LessThan src1 src2 dst]
  TracIr.LessThanOrEqualTo dst src1 src2 -> pure [LessThanOrEqualTo src1 src2 dst]
  TracIr.Equals dst src1 src2 -> pure [Equals src1 src2 dst]
  TracIr.IntConstant var i -> pure [IntConstant i var]
  TracIr.BoolConstant var i -> pure [BoolConstant i var]
  TracIr.StringConstant var i -> pure [StringConstant i var]
  TracIr.Not dst src -> pure $ generateUnaryStatement Not dst src
  TracIr.Negate dst src -> pure $ generateUnaryStatement Negate dst src
  TracIr.New dst type' -> pure [New type' dst]
  TracIr.Default dst type' -> pure [Default type' dst]
  TracIr.IsVoid dst src -> pure $ generateUnaryStatement IsVoid dst src
  TracIr.Dispatch res rec recType t m as -> pure [Dispatch res rec recType t m as]
  TracIr.Jump l -> pure [Jump l]
  TracIr.TracLabel l -> pure [TwacLabel l]
  TracIr.Return v -> pure [Return v]
  TracIr.Comment c -> pure [Comment c]
  TracIr.ConditionalJump v l _ -> pure [ConditionalJump v l]
  TracIr.Assign dst src -> pure [Assign src dst]
  TracIr.Case _ caseVariable jumpTable _ -> pure [TwacCase caseVariable jumpTable]
  TracIr.TracInternal internal -> pure [TwacInternal internal]
  TracIr.Abort line reason -> pure [Abort line reason]

-- I cannot tell if this is the most beautiful or the most ugly code I have
-- written. I think I am leaning towards the most ugly. This is hyperbole of
-- course, but it's... certainly something. Honestly, I constructed it through
-- iterating away type errors.
tracToTwac :: TracIr.Trac v -> State Temporary (Twac v)
tracToTwac trac =
  concatMap unsequence <$> mapM (mapM generateTwacStatement) trac

generateTwacMethod :: TracIr.TracMethod Variable -> State Temporary (TwacMethod Variable)
generateTwacMethod (TracIr.TracMethod methodName body formals temporaryCount) = do
  modify (\(Temporary label _) -> Temporary label temporaryCount)
  body' <- tracToTwac body
  temporaryCount' <- gets (\(Temporary _ temporary) -> temporary)
  pure $ TwacMethod methodName body' formals temporaryCount'

generateTwac :: TracIr.TracIr Variable -> Temporary -> (TwacIr Variable, Temporary)
generateTwac (TracIr.TracIr impMap typeDetailsMap) =
  runState $
    TwacIr
      <$> traverse (traverse (traverse generateTwacMethod)) impMap
      <*> pure typeDetailsMap
