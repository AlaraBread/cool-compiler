{-# LANGUAGE NamedFieldPuns #-}

module Twac where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import InputIr (Formal, Type)
import qualified InputIr
import Trac (Label, Temporary, Variable)
import qualified Trac
import Util

type Twac v = [Lined (TwacStatement v)]

-- TwacI means TWAC initial, i.e. our first version of TWAC in the pipeline.
type TwacI = Twac Variable

type TwacIStatement = TwacStatement Variable

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
  | IntConstant Int v
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
  | Abort Int (Trac.AbortReason v)
  | TwacInternal InputIr.Internal

data TwacIr v = TwacIr
  { implementationMap :: Map.Map InputIr.Type [InputIr.ImplementationMapEntry (TwacMethod v)],
    typeDetails :: Trac.TypeDetailsMap
  }

type TwacIIr = TwacIr Variable

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
          New (InputIr.Type t) dst -> show dst ++ " <- new " ++ t
          Default (InputIr.Type t) dst -> show dst ++ " <- default " ++ t
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

generateUnaryStatement :: (Variable -> TwacIStatement) -> Variable -> Variable -> [TwacIStatement]
generateUnaryStatement op dst src =
  [Copy src dst, op dst]

generateBinaryStatement :: (Variable -> Variable -> TwacIStatement) -> Variable -> Variable -> Variable -> [TwacIStatement]
generateBinaryStatement op dst src1 src2 =
  [Copy src1 dst, op src2 dst]

generateTwacStatement :: Trac.TracStatement -> State Temporary [TwacIStatement]
generateTwacStatement tracStatement = case tracStatement of
  Trac.Add dst src1 src2 -> pure $ generateBinaryStatement Add dst src1 src2
  Trac.Subtract dst src1 src2 -> pure $ generateBinaryStatement Subtract dst src1 src2
  Trac.Multiply dst src1 src2 -> pure $ generateBinaryStatement Multiply dst src1 src2
  Trac.Divide dst src1 src2 -> pure $ generateBinaryStatement Divide dst src1 src2
  Trac.LessThan dst src1 src2 -> pure [LessThan src1 src2 dst]
  Trac.LessThanOrEqualTo dst src1 src2 -> pure [LessThanOrEqualTo src1 src2 dst]
  Trac.Equals dst src1 src2 -> pure [Equals src1 src2 dst]
  Trac.IntConstant var i -> pure [IntConstant i var]
  Trac.BoolConstant var i -> pure [BoolConstant i var]
  Trac.StringConstant var i -> pure [StringConstant i var]
  Trac.Not dst src -> pure $ generateUnaryStatement Not dst src
  Trac.Negate dst src -> pure $ generateUnaryStatement Negate dst src
  Trac.New dst type' -> pure [New type' dst]
  Trac.Default dst type' -> pure [Default type' dst]
  Trac.IsVoid dst src -> pure $ generateUnaryStatement IsVoid dst src
  Trac.Dispatch res rec recType t m as -> pure [Dispatch res rec recType t m as]
  Trac.Jump l -> pure [Jump l]
  Trac.TracLabel l -> pure [TwacLabel l]
  Trac.Return v -> pure [Return v]
  Trac.Comment c -> pure [Comment c]
  Trac.ConditionalJump v l -> pure [ConditionalJump v l]
  Trac.Assign dst src -> pure [Assign src dst]
  Trac.Case _ caseVariable jumpTable -> pure [TwacCase caseVariable jumpTable]
  Trac.TracInternal internal -> pure [TwacInternal internal]
  Trac.Abort line reason -> pure [Abort line reason]

-- I cannot tell if this is the most beautiful or the most ugly code I have
-- written. I think I am leaning towards the most ugly. This is hyperbole of
-- course, but it's... certainly something. Honestly, I constructed it through
-- iterating away type errors.
tracToTwac :: Trac.Trac -> State Temporary TwacI
tracToTwac trac =
  concatMap unsequence <$> mapM (mapM generateTwacStatement) trac

generateTwacMethod :: Trac.TracMethod -> State Temporary (TwacMethod Variable)
generateTwacMethod (Trac.TracMethod methodName body formals temporaryCount) = do
  modify (\(Trac.Temporary label _) -> Trac.Temporary label temporaryCount)
  body' <- tracToTwac body
  temporaryCount' <- gets (\(Trac.Temporary _ temporary) -> temporary)
  pure $ TwacMethod methodName body' formals temporaryCount'

generateTwac :: Trac.TracIr -> Temporary -> (TwacIr Variable, Temporary)
generateTwac (Trac.TracIr impMap typeDetailsMap) =
  runState $
    TwacIr
      <$> traverse (traverse (traverse generateTwacMethod)) impMap
      <*> pure typeDetailsMap
