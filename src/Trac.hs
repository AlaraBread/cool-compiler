{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Trac where

import Control.Monad.State
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Distribution.Simple.Utils (lowercase)
import qualified InputIr
import Util

data TracIr = TracIr {implementationMap :: Map.Map InputIr.Type [TracMethod], constructorMap :: Map.Map InputIr.Type Trac}
  deriving (Show)

data TracMethod = TracMethod {methodName :: String, body :: Trac, formals :: [InputIr.Formal], returnVariable :: Variable}
  deriving (Show)

type Trac = [Lined TracStatement]

data TracStatement
  = Add Variable Variable Variable
  | Subtract Variable Variable Variable
  | Multiply Variable Variable Variable
  | Divide Variable Variable Variable
  | LessThan Variable Variable Variable
  | LessThanOrEqualTo Variable Variable Variable
  | Equals Variable Variable Variable
  | IntConstant Variable Int32
  | BoolConstant Variable Bool
  | StringConstant Variable String
  | Not Variable Variable
  | Negate Variable Variable
  | New Variable InputIr.Type
  | Default Variable InputIr.Type
  | IsVoid Variable Variable
  | Dispatch
      { dispatchResult :: Variable,
        dispatchReceiver :: Variable,
        dispatchType :: Maybe InputIr.Type,
        dispatchMethod :: String,
        dispatchArgs :: [Variable]
      }
  | Jump Label
  | TracLabel Label
  | Return Variable
  | Comment String
  | ConditionalJump Variable Label
  | Assign Variable Variable
  | Case (Trac, Variable) [CaseElement]

data CaseElement = CaseElement Variable InputIr.Type (Trac, Variable)

instance Show TracStatement where
  show t = case t of
    Add a b c -> showBinary a b c "+"
    Subtract a b c -> showBinary a b c "-"
    Multiply a b c -> showBinary a b c "*"
    Divide a b c -> showBinary a b c "/"
    LessThan a b c -> showBinary a b c "<"
    LessThanOrEqualTo a b c -> showBinary a b c "<="
    Equals a b c -> showBinary a b c "="
    IntConstant v i -> show v ++ " <- int " ++ show i
    BoolConstant v b -> show v ++ " <- bool " ++ lowercase (show b)
    StringConstant v s -> show v ++ " <- string\n" ++ s
    Not a b -> showUnary a b "not"
    Negate a b -> showUnary a b "~"
    New a (InputIr.Type t) -> show a ++ " <- new " ++ t
    Default a (InputIr.Type t) -> show a ++ " <- default " ++ t
    IsVoid a b -> showUnary a b "isvoid"
    Dispatch
      { dispatchResult,
        dispatchReceiver,
        dispatchType,
        dispatchMethod,
        dispatchArgs
      } -> show dispatchResult ++ " <- call " ++ dispatchMethod ++ " " ++ unwords (map show dispatchArgs)
    Jump l -> "jmp " ++ show l
    TracLabel l -> "label " ++ show l
    Return a -> "return " ++ show a
    Comment msg -> "comment " ++ msg
    ConditionalJump a l -> "bt " ++ show a ++ " " ++ show l
    Assign a b -> show a ++ " <- " ++ show b
    Case e elements -> "comment case :3" -- dont bother outputting case statements for now

showBinary :: Variable -> Variable -> Variable -> String -> String
showBinary a b c op = show a ++ " <- " ++ op ++ " " ++ show b ++ " " ++ show c

showUnary :: Variable -> Variable -> String -> String
showUnary a b op = show a ++ " <- " ++ op ++ " " ++ show b

data Variable = StringV String | TemporaryV Temporary

instance Show Variable where
  show (StringV s) = s
  show (TemporaryV (Temporary t)) = "temp" ++ show t

newtype Label = Label String

instance Show Label where
  show (Label l) = l

newtype Temporary = Temporary Int
  deriving (Show)

showTrac trac = unlines (map show trac)

getVariable :: State Temporary Variable
getVariable = state $ \(Temporary i) -> (TemporaryV $ Temporary $ i + 1, Temporary $ i + 1)

getLabel :: State Temporary Label
getLabel = state $ \(Temporary i) -> (Label $ "l" ++ show (i + 1), Temporary $ i + 1)

generateTracExpr :: InputIr.Typed InputIr.Expr -> State Temporary (Trac, Variable)
generateTracExpr
  InputIr.Typed
    { InputIr.type',
      InputIr.item = Lined line_number expr
    } =
    let lined :: InputIr.Typed InputIr.Expr -> a -> Lined a
        lined e = Lined (line $ InputIr.item e)

        lined' :: a -> Lined a
        lined' = Lined line_number

        compose2 :: (b -> c) -> (a -> a -> b) -> (a -> a -> c)
        compose2 g f p1 p2 = g $ f p1 p2

        compose3 :: (b -> c) -> (a -> a -> a -> b) -> (a -> a -> a -> c)
        compose3 g f p1 p2 p3 = g $ f p1 p2 p3

        linedUnary = compose2 lined'
        linedBinary = compose3 lined'
     in case expr of
          InputIr.Assign
            InputIr.Identifier
              { InputIr.lexeme
              }
            exp ->
              do
                (trac, variable) <- generateTracExpr exp
                let lhs = StringV lexeme
                return (trac ++ [lined' $ Assign lhs variable], lhs)
          InputIr.DynamicDispatch
            { InputIr.dynamicDispatchLhs = receiver,
              InputIr.dynamicDispatchMethod = method,
              InputIr.dynamicDispachArgs = args
            } -> do
              temp <- getVariable
              dispatchArgs' <- mapM generateTracExpr args
              let dispatchArgsTrac = concatMap fst dispatchArgs'
              let dispatchArgsV = map snd dispatchArgs'
              (receiverTrac, receiverV) <- generateTracExpr receiver
              pure
                ( dispatchArgsTrac
                    ++ receiverTrac
                    ++ [ lined' $
                           Dispatch
                             { dispatchResult = temp,
                               dispatchMethod = InputIr.lexeme method,
                               dispatchReceiver = receiverV,
                               dispatchType = Nothing,
                               dispatchArgs = dispatchArgsV
                             }
                       ],
                  temp
                )
          InputIr.StaticDispatch
            { InputIr.staticDispatchLhs = receiver,
              InputIr.dynamicDispatchType = type',
              InputIr.dynamicDispatchMethod = method,
              InputIr.dynamicDispatchArgs = args
            } -> do
              temp <- getVariable
              dispatchArgs' <- mapM generateTracExpr args
              let dispatchArgsTrac = concatMap fst dispatchArgs'
              let dispatchArgsV = map snd dispatchArgs'
              (receiverTrac, receiverV) <- generateTracExpr receiver
              pure
                ( dispatchArgsTrac
                    ++ receiverTrac
                    ++ [ lined' $
                           Dispatch
                             { dispatchResult = temp,
                               dispatchMethod = InputIr.lexeme method,
                               dispatchReceiver = receiverV,
                               dispatchType = Just $ InputIr.Type $ InputIr.lexeme type',
                               dispatchArgs = dispatchArgsV
                             }
                       ],
                  temp
                )
          InputIr.SelfDispatch
            { InputIr.selfDispatchMethod,
              InputIr.selfDispatchArgs
            } -> do
              temp <- getVariable
              dispatchArgs' <- mapM generateTracExpr selfDispatchArgs
              let dispatchArgsTrac = concatMap fst dispatchArgs'
              let dispatchArgsV = map snd dispatchArgs'
              pure
                ( dispatchArgsTrac
                    ++ [ lined' $
                           Dispatch
                             { dispatchResult = temp,
                               dispatchMethod = InputIr.lexeme selfDispatchMethod,
                               dispatchReceiver = StringV "self",
                               dispatchType = Nothing,
                               dispatchArgs = dispatchArgsV
                             }
                       ],
                  temp
                )
          InputIr.If
            { InputIr.ifPredicate,
              InputIr.trueBody,
              InputIr.falseBody
            } -> do
              (predicateTrac, predicateV) <- generateTracExpr ifPredicate
              (trueTrac, trueV) <- generateTracExpr trueBody
              (falseTrac, falseV) <- generateTracExpr falseBody
              trueLabel <- getLabel
              trueEndLabel <- getLabel
              bodyV <- getVariable
              let trueTrac' = lined trueBody (TracLabel trueLabel) : trueTrac ++ [lined trueBody (Assign bodyV trueV), lined trueBody (TracLabel trueEndLabel)]
              let falseTrac' = falseTrac ++ [lined falseBody (Assign bodyV falseV), lined falseBody $ Jump trueEndLabel]
              return
                ( predicateTrac
                    ++ [lined ifPredicate $ ConditionalJump predicateV trueLabel]
                    ++ falseTrac'
                    ++ trueTrac',
                  bodyV
                )
          InputIr.While
            { InputIr.whilePredicate,
              InputIr.whileBody
            } -> do
              (predicateTrac, predicateV) <- generateTracExpr whilePredicate
              (bodyTrac, bodyV) <- generateTracExpr whileBody
              outV <- getVariable
              whileStart <- getLabel
              endLabel <- getLabel
              notPredicateV <- getVariable
              pure
                ( [lined whileBody $ TracLabel whileStart]
                    ++ predicateTrac
                    ++ [lined whilePredicate $ Not notPredicateV predicateV]
                    ++ [lined whilePredicate $ ConditionalJump notPredicateV endLabel]
                    ++ bodyTrac
                    ++ [lined whilePredicate $ Jump whileStart]
                    ++ [lined whilePredicate $ TracLabel endLabel]
                    ++ [lined whilePredicate $ Default outV $ InputIr.Type "Object"],
                  outV
                )
          InputIr.Block expressions -> do
            expressions <- traverse generateTracExpr expressions
            pure
              ( concatMap fst expressions,
                snd $ last expressions -- this will crash on an empty block
              )
          InputIr.New InputIr.Identifier {InputIr.lexeme = typeName} -> do
            t <- getVariable
            pure ([lined' $ New t $ InputIr.Type typeName], t)
          InputIr.IsVoid exp -> unaryOperation exp $ linedUnary IsVoid
          InputIr.Plus a b -> binaryOperation a b $ linedBinary Add
          InputIr.Minus a b -> binaryOperation a b $ linedBinary Subtract
          InputIr.Times a b -> binaryOperation a b $ linedBinary Multiply
          InputIr.Divide a b -> binaryOperation a b $ linedBinary Divide
          InputIr.LessThan a b -> binaryOperation a b $ linedBinary LessThan
          InputIr.LessThanOrEqualTo a b -> binaryOperation a b $ linedBinary LessThanOrEqualTo
          InputIr.Equal a b -> binaryOperation a b $ linedBinary Equals
          InputIr.Not exp -> unaryOperation exp $ linedUnary Not
          InputIr.Negate exp -> unaryOperation exp $ linedUnary Negate
          InputIr.IntegerConstant i -> constant line_number IntConstant i
          InputIr.StringConstant s -> constant line_number StringConstant s
          InputIr.BooleanConstant b -> constant line_number BoolConstant b
          InputIr.Variable InputIr.Identifier {InputIr.lexeme} -> pure ([], StringV lexeme)
          InputIr.Let bindings body -> do
            -- we need to restore the old values of bindings after the let statement
            bindings <-
              traverse
                ( \InputIr.LetBinding
                     { InputIr.letBindingName = InputIr.Identifier {InputIr.lexeme = name, InputIr.line = bindingLine},
                       InputIr.letBindingRhs = rhs,
                       InputIr.letBindingType' = InputIr.Identifier {InputIr.lexeme = type'}
                     } ->
                      let bindingV = StringV name
                       in case rhs of
                            Just rhs -> do
                              (rhsTrac, rhsV) <- generateTracExpr rhs
                              tmp <- getVariable
                              pure
                                ( rhsTrac
                                    ++ [ lined rhs $ Assign bindingV rhsV,
                                         Lined bindingLine $ Assign tmp rhsV
                                       ],
                                  [lined' $ Assign bindingV tmp]
                                )
                            Nothing -> do
                              tmp <- getVariable
                              pure
                                ( [ lined' $ Default bindingV (InputIr.Type type'),
                                    lined' $ Default tmp (InputIr.Type type')
                                  ],
                                  [lined' $ Assign bindingV tmp]
                                )
                )
                bindings
            let bindingInitTrac = concatMap fst bindings
            let bindingResetTrac = concatMap snd bindings
            (bodyTrac, bodyV) <- generateTracExpr body
            pure (bindingInitTrac ++ bodyTrac ++ bindingResetTrac, bodyV)
          InputIr.Case e elements -> do
            (eTrac, eV) <- generateTracExpr e
            a <-
              mapM
                ( \InputIr.CaseElement
                     { InputIr.caseElementVariable,
                       InputIr.caseElementType,
                       InputIr.caseElementBody
                     } -> do
                      body <- generateTracExpr caseElementBody
                      pure $ CaseElement (StringV $ InputIr.lexeme caseElementVariable) (InputIr.Type $ InputIr.lexeme caseElementType) body
                )
                elements
            pure (eTrac, eV)
          InputIr.IOInInt -> ([lined' $ Comment "IO.in_int"],) <$> getVariable
          InputIr.IOInString -> ([lined' $ Comment "IO.in_string"],) <$> getVariable
          InputIr.IOOutInt -> ([lined' $ Comment "IO.out_int"],) <$> getVariable
          InputIr.IOOutString -> ([lined' $ Comment "IO.out_string"],) <$> getVariable
          InputIr.ObjectAbort -> ([lined' $ Comment "Object.abort"],) <$> getVariable
          InputIr.ObjectCopy -> ([lined' $ Comment "Object.copy"],) <$> getVariable
          InputIr.ObjectTypeName -> ([lined' $ Comment "Object.type_name"],) <$> getVariable
          InputIr.StringConcat -> ([lined' $ Comment "String.concat"],) <$> getVariable
          InputIr.StringLength -> ([lined' $ Comment "String.length"],) <$> getVariable
          InputIr.StringSubstr -> ([lined' $ Comment "String.substr"],) <$> getVariable

binaryOperation ::
  InputIr.Typed InputIr.Expr ->
  InputIr.Typed InputIr.Expr ->
  (Variable -> Variable -> Variable -> Lined TracStatement) ->
  State Temporary (Trac, Variable)
binaryOperation a b op = do
  (aTrac, aV) <- generateTracExpr a
  (bTrac, bV) <- generateTracExpr b
  resultV <- getVariable
  pure (aTrac ++ bTrac ++ [op resultV aV bV], resultV)

unaryOperation ::
  InputIr.Typed InputIr.Expr ->
  (Variable -> Variable -> Lined TracStatement) ->
  State Temporary (Trac, Variable)
unaryOperation exp op = do
  (trac, v) <- generateTracExpr exp
  resultV <- getVariable
  pure (trac ++ [op resultV v], resultV)

constant :: Int -> (Variable -> a -> TracStatement) -> a -> State Temporary (Trac, Variable)
constant line statement c = do
  resultV <- getVariable
  pure ([Lined line $ statement resultV c], resultV)

generateTracMethod :: InputIr.Type -> InputIr.Method -> State Temporary TracMethod
generateTracMethod (InputIr.Type typeName) (InputIr.Method {InputIr.methodName, InputIr.methodFormals, InputIr.methodBody}) = do
  (trac, v) <- generateTracExpr methodBody
  pure
    TracMethod
      { methodName = InputIr.lexeme methodName,
        body =
          Lined (InputIr.line methodName) (TracLabel (Label $ typeName ++ "_" ++ InputIr.lexeme methodName))
            : trac
            ++ [Lined (InputIr.line methodName) $ Return v],
        formals = methodFormals,
        returnVariable = v
      }

generateTracConstructor :: [InputIr.Attribute] -> State Temporary Trac
generateTracConstructor attrs = do
  attrs' <-
    traverse
      ( \InputIr.Attribute
           { InputIr.attrName = InputIr.Identifier {InputIr.lexeme, InputIr.line},
             InputIr.attrType,
             InputIr.attrRhs
           } -> case attrRhs of
            Just e -> do
              (trac, v) <- generateTracExpr e
              pure $ trac ++ [Lined line $ Assign (StringV lexeme) v]
            Nothing -> pure [Lined line $ Default (StringV lexeme) attrType]
      )
      attrs
  pure $ concat attrs'

generateTrac :: InputIr.InputIr -> TracIr
generateTrac (InputIr.InputIr classMap implMap parentMap ast) =
  evalState
    ( do
        implMap' <- sequence $ Map.mapWithKey (mapM . generateTracMethod) $ Map.map (map snd) implMap
        constructorMap <- mapM generateTracConstructor classMap
        pure TracIr {implementationMap = implMap', constructorMap = constructorMap}
    )
    ( Temporary
        0
    )
