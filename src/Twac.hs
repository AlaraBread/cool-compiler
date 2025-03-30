{-# LANGUAGE NamedFieldPuns #-}

module Twac where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import InputIr (Formal, Type)
import qualified InputIr
import qualified InputIr as Twac
import Trac (Label, Temporary, Variable, getLabel)
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
  | LessThan v v
  | LessThanOrEqualTo v v
  | Equals v v
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
  | TwacCase v CaseJmpTable
  | Abort Int String

-- NOTE: this should include *every single* type.
type CaseJmpTable = Map.Map Type Label

data TwacIr v = TwacIr
  { implementationMap :: Map.Map InputIr.Type [TwacMethod v],
    constructorMap :: Map.Map Type (Twac v),
    typeDetails :: Trac.TypeDetailsMap
  }

type TwacIIr = TwacIr Variable

data TwacMethod v = TwacMethod {methodName :: String, body :: Twac v, formals :: [Formal], temporaryCount :: Int}

instance (Show v) => Show (TwacStatement v) where
  show t =
    let showUnary op dst = show dst ++ " <- " ++ op ++ show dst
        showBinary src dst op = show dst ++ " <- " ++ show dst ++ " " ++ op ++ " " ++ show src
        showImmediate immediate dst = show dst ++ " <- " ++ show immediate
     in case t of
          Add src dst -> showBinary src dst "+"
          Subtract src dst -> showBinary src dst "-"
          Multiply src dst -> showBinary src dst "*"
          Divide src dst -> showBinary src dst "/"
          LessThan src dst -> showBinary src dst "<"
          LessThanOrEqualTo src dst -> showBinary src dst "<="
          Equals src dst -> showBinary src dst "="
          IntConstant i dst -> showImmediate i dst
          BoolConstant i dst -> showImmediate i dst
          StringConstant i dst -> showImmediate i dst
          Not dst -> showUnary "not " dst
          Negate dst -> showUnary "~" dst
          New (InputIr.Type t) dst -> show dst ++ " <- new " ++ t
          Default (InputIr.Type t) dst -> show dst ++ " <- default " ++ t
          IsVoid dst -> showUnary "isVoid " dst
          -- TODO: this is incomplete
          Dispatch {dispatchResult, dispatchMethod, dispatchReceiver} -> show dispatchReceiver ++ "." ++ dispatchMethod
          Jump lbl -> "jmp " ++ show lbl
          TwacLabel lbl -> "label " ++ show lbl
          Return var -> "return " ++ show var
          Comment str -> "comment " ++ str
          ConditionalJump var lbl -> "bt " ++ show var ++ show lbl
          Assign src dst -> show dst ++ " <- " ++ show src
          Copy src dst -> "copy " ++ show src ++ " " ++ show dst
          -- TODO: this is incomplete
          TwacCase var _ -> "case " ++ show var
          Abort line str -> "abort " ++ show line ++ ": " ++ str

showTwac twac = unlines (map show twac)

generateUnaryStatement :: (Variable -> TwacIStatement) -> Variable -> Variable -> [TwacIStatement]
generateUnaryStatement op dst src =
  [Copy src dst, op dst]

generateBinaryStatement :: (Variable -> Variable -> TwacIStatement) -> Variable -> Variable -> Variable -> [TwacIStatement]
generateBinaryStatement op dst src1 src2 =
  [Copy src1 dst, op src2 dst]

generateTwacStatement :: ([Type] -> Map.Map Type (Maybe Type)) -> Trac.TracStatement -> State Temporary [TwacIStatement]
generateTwacStatement pickLowestParents tracStatement = case tracStatement of
  Trac.Add dst src1 src2 -> pure $ generateBinaryStatement Add dst src1 src2
  Trac.Subtract dst src1 src2 -> pure $ generateBinaryStatement Subtract dst src1 src2
  Trac.Multiply dst src1 src2 -> pure $ generateBinaryStatement Multiply dst src1 src2
  Trac.Divide dst src1 src2 -> pure $ generateBinaryStatement Divide dst src1 src2
  Trac.LessThan dst src1 src2 -> pure $ generateBinaryStatement LessThan dst src1 src2
  Trac.LessThanOrEqualTo dst src1 src2 -> pure $ generateBinaryStatement LessThanOrEqualTo dst src1 src2
  Trac.Equals dst src1 src2 -> pure $ generateBinaryStatement Equals dst src1 src2
  Trac.IntConstant var i -> pure [IntConstant i var]
  Trac.BoolConstant var i -> pure [BoolConstant i var]
  Trac.StringConstant var i -> pure [StringConstant i var]
  Trac.Not dst src -> pure $ generateUnaryStatement Not dst src
  Trac.Negate dst src -> pure $ generateUnaryStatement Negate dst src
  Trac.New dst type' -> pure [New type' dst]
  Trac.Default dst type' -> pure [Default type' dst]
  Trac.IsVoid dst src -> pure $ generateUnaryStatement Not dst src
  Trac.Dispatch res rec recType t m as -> pure [Dispatch res rec recType t m as]
  Trac.Jump l -> pure [Jump l]
  Trac.TracLabel l -> pure [TwacLabel l]
  Trac.Return v -> pure [Return v]
  Trac.Comment c -> pure [Comment c]
  Trac.ConditionalJump v l -> pure [ConditionalJump v l]
  Trac.Assign dst src -> pure [Assign src dst]
  Trac.Case outputVariable (initializer, caseOf) caseElements -> do
    initializer' <- tracToTwac pickLowestParents initializer
    let types = fmap (\(Trac.CaseElement t _) -> t) caseElements

    caseElementLabels <- mapM (\(Trac.CaseElement {}) -> getLabel) caseElements

    errorCaseLabel <- getLabel
    -- TODO: include the line number
    let errorCase = [TwacLabel errorCaseLabel, Abort 0 "no match found for case :<"]

    terminatingLabel <- getLabel

    caseElementBodies <-
      mapM
        ( \(Trac.CaseElement _ (trac, v), l) ->
            (++ [Lined 0 $ TwacLabel l, Lined 0 $ Assign v outputVariable, Lined 0 $ Jump terminatingLabel])
              <$> tracToTwac pickLowestParents trac
        )
        $ zip caseElements caseElementLabels

    let caseLabelTable = Map.fromList $ zip types caseElementLabels
    let jumpTable = maybe errorCaseLabel (caseLabelTable Map.!) <$> pickLowestParents types

    -- TODO: THIS LOSES LINE NUMBERS ON THE ELEMENT BODIES. THAT IS BAD.
    pure $
      [Assign caseOf outputVariable, TwacCase outputVariable jumpTable]
        ++ concatMap (fmap item) caseElementBodies
        ++ errorCase

-- I cannot tell if this is the most beautiful or the most ugly code I have
-- written. I think I am leaning towards the most ugly. This is hyperbole of
-- course, but it's... certainly something. Honestly, I constructed it through
-- iterating away type errors.
tracToTwac :: ([Type] -> Map.Map Type (Maybe Type)) -> Trac.Trac -> State Temporary TwacI
tracToTwac pickLowestParents trac =
  concatMap unsequence <$> mapM (mapM (generateTwacStatement pickLowestParents)) trac

generateTwacMethod :: ([Type] -> Map.Map Type (Maybe Type)) -> Trac.TracMethod -> State Temporary (TwacMethod Variable)
generateTwacMethod pickLowestParents (Trac.TracMethod methodName body formals temporaryCount) = do
  modify (\(Trac.Temporary l t) -> Trac.Temporary l temporaryCount)
  body' <- tracToTwac pickLowestParents body
  temporaryCount' <- gets (\(Trac.Temporary l t) -> t)
  pure $ TwacMethod methodName body' formals temporaryCount'

generateTwac :: ([Type] -> Map.Map Type (Maybe Type)) -> Trac.TracIr -> Temporary -> (TwacIr Variable, Temporary)
generateTwac pickLowestParents (Trac.TracIr impMap constructorMap typeDetailsMap) =
  runState $
    TwacIr
      <$> traverse (traverse $ generateTwacMethod pickLowestParents) impMap
      <*> traverse (tracToTwac pickLowestParents) constructorMap
      <*> pure typeDetailsMap
