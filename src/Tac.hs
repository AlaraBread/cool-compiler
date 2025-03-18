{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Tac where

import Control.Monad.State
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Distribution.Simple.Utils (lowercase)
import qualified InputIr
import Util

data TacIr = TacIr {implementationMap :: Map.Map InputIr.Type [TacMethod], constructorMap :: Map.Map InputIr.Type Tac}
  deriving (Show)

data TacMethod = TacMethod {methodName :: String, body :: Tac, formals :: [InputIr.Formal], returnVariable :: Variable}
  deriving (Show)

type Tac = [Lined TacStatement]

data TacStatement
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
  | TacLabel Label
  | Return Variable
  | Comment String
  | ConditionalJump Variable Label
  | Assign Variable Variable
  | Case (Tac, Variable) [CaseElement]

data CaseElement = CaseElement Variable InputIr.Type (Tac, Variable)

instance Show TacStatement where
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
    TacLabel l -> "label " ++ show l
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

showTac tac = unlines (map show tac)

getVariable :: State Temporary Variable
getVariable = state $ \(Temporary i) -> (TemporaryV $ Temporary $ i + 1, Temporary $ i + 1)

getLabel :: State Temporary Label
getLabel = state $ \(Temporary i) -> (Label $ "l" ++ show (i + 1), Temporary $ i + 1)

generateTacExpr :: InputIr.Typed InputIr.Expr -> State Temporary (Tac, Variable)
generateTacExpr
  InputIr.Typed
    { InputIr.type',
      InputIr.item = Lined line_number expr
    } =
    let
      lined :: InputIr.Typed InputIr.Expr -> a -> Lined a
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
                (tac, variable) <- generateTacExpr exp
                let lhs = StringV lexeme
                return (tac ++ [lined' $ Assign lhs variable], lhs)
          InputIr.DynamicDispatch
            { InputIr.dynamicDispatchLhs = receiver,
              InputIr.dynamicDispatchMethod = method,
              InputIr.dynamicDispachArgs = args
            } -> do
              temp <- getVariable
              dispatchArgs' <- mapM generateTacExpr args
              let dispatchArgsTac = concatMap fst dispatchArgs'
              let dispatchArgsV = map snd dispatchArgs'
              (receiverTac, receiverV) <- generateTacExpr receiver
              pure
                ( dispatchArgsTac
                    ++ receiverTac
                    ++ [ lined' $ Dispatch
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
              dispatchArgs' <- mapM generateTacExpr args
              let dispatchArgsTac = concatMap fst dispatchArgs'
              let dispatchArgsV = map snd dispatchArgs'
              (receiverTac, receiverV) <- generateTacExpr receiver
              pure
                ( dispatchArgsTac
                    ++ receiverTac
                    ++ [ lined' $ Dispatch
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
              dispatchArgs' <- mapM generateTacExpr selfDispatchArgs
              let dispatchArgsTac = concatMap fst dispatchArgs'
              let dispatchArgsV = map snd dispatchArgs'
              pure
                ( dispatchArgsTac
                    ++ [ lined' $ Dispatch
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
              (predicateTac, predicateV) <- generateTacExpr ifPredicate
              (trueTac, trueV) <- generateTacExpr trueBody
              (falseTac, falseV) <- generateTacExpr falseBody
              trueLabel <- getLabel
              trueEndLabel <- getLabel
              bodyV <- getVariable
              let trueTac' = lined trueBody (TacLabel trueLabel) : trueTac ++ [lined trueBody (Assign bodyV trueV), lined trueBody (TacLabel trueEndLabel)]
              let falseTac' = falseTac ++ [lined falseBody (Assign bodyV falseV), lined falseBody $ Jump trueEndLabel]
              return
                ( predicateTac
                    ++ [lined ifPredicate $ ConditionalJump predicateV trueLabel]
                    ++ falseTac'
                    ++ trueTac',
                  bodyV
                )
          InputIr.While
            { InputIr.whilePredicate,
              InputIr.whileBody
            } -> do
              (predicateTac, predicateV) <- generateTacExpr whilePredicate
              (bodyTac, bodyV) <- generateTacExpr whileBody
              outV <- getVariable
              whileStart <- getLabel
              endLabel <- getLabel
              notPredicateV <- getVariable
              pure
                ( [lined whileBody $ TacLabel whileStart]
                    ++ predicateTac
                    ++ [lined whilePredicate $ Not notPredicateV predicateV]
                    ++ [lined whilePredicate $ ConditionalJump notPredicateV endLabel]
                    ++ bodyTac
                    ++ [lined whilePredicate $ Jump whileStart]
                    ++ [lined whilePredicate $ TacLabel endLabel]
                    ++ [lined whilePredicate $ Default outV $ InputIr.Type "Object"],
                  outV
                )
          InputIr.Block expressions -> do
            expressions <- traverse generateTacExpr expressions
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
                              (rhsTac, rhsV) <- generateTacExpr rhs
                              tmp <- getVariable
                              pure
                                ( rhsTac
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
            let bindingInitTac = concatMap fst bindings
            let bindingResetTac = concatMap snd bindings
            (bodyTac, bodyV) <- generateTacExpr body
            pure (bindingInitTac ++ bodyTac ++ bindingResetTac, bodyV)
          InputIr.Case e elements -> do
            (eTac, eV) <- generateTacExpr e
            a <-
              mapM
                ( \InputIr.CaseElement
                     { InputIr.caseElementVariable,
                       InputIr.caseElementType,
                       InputIr.caseElementBody
                     } -> do
                      body <- generateTacExpr caseElementBody
                      pure $ CaseElement (StringV $ InputIr.lexeme caseElementVariable) (InputIr.Type $ InputIr.lexeme caseElementType) body
                )
                elements
            pure (eTac, eV)
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
  (Variable -> Variable -> Variable -> Lined TacStatement) ->
  State Temporary (Tac, Variable)
binaryOperation a b op = do
  (aTac, aV) <- generateTacExpr a
  (bTac, bV) <- generateTacExpr b
  resultV <- getVariable
  pure (aTac ++ bTac ++ [op resultV aV bV], resultV)

unaryOperation ::
  InputIr.Typed InputIr.Expr ->
  (Variable -> Variable -> Lined TacStatement) ->
  State Temporary (Tac, Variable)
unaryOperation exp op = do
  (tac, v) <- generateTacExpr exp
  resultV <- getVariable
  pure (tac ++ [op resultV v], resultV)

constant :: Int -> (Variable -> a -> TacStatement) -> a -> State Temporary (Tac, Variable)
constant line statement c = do
  resultV <- getVariable
  pure ([Lined line $ statement resultV c], resultV)

generateTacMethod :: InputIr.Type -> InputIr.Method -> State Temporary TacMethod
generateTacMethod (InputIr.Type typeName) (InputIr.Method {InputIr.methodName, InputIr.methodFormals, InputIr.methodBody}) = do
  (tac, v) <- generateTacExpr methodBody
  pure
    TacMethod
      { methodName = InputIr.lexeme methodName,
        body =
          Lined (InputIr.line methodName) (TacLabel (Label $ typeName ++ "_" ++ InputIr.lexeme methodName))
            : tac
            ++ [Lined (InputIr.line methodName) $ Return v],
        formals = methodFormals,
        returnVariable = v
      }

generateTacConstructor :: [InputIr.Attribute] -> State Temporary Tac
generateTacConstructor attrs = do
  attrs' <-
    traverse
      ( \InputIr.Attribute
           { InputIr.attrName = InputIr.Identifier {InputIr.lexeme, InputIr.line},
             InputIr.attrType,
             InputIr.attrRhs
           } -> case attrRhs of
            Just e -> do
              (tac, v) <- generateTacExpr e
              pure [Lined line $ Assign (StringV lexeme) v]
            Nothing -> pure [Lined line $ Default (StringV lexeme) attrType]
      )
      attrs
  pure $ concat attrs'

generateTac :: InputIr.InputIr -> TacIr
generateTac (InputIr.InputIr classMap implMap parentMap ast) =
  evalState
    ( do
        implMap' <- sequence $ Map.mapWithKey (mapM . generateTacMethod) $ Map.map (map snd) implMap
        constructorMap <- mapM generateTacConstructor classMap
        pure TacIr {implementationMap = implMap', constructorMap = constructorMap}
    )
    ( Temporary
        0
    )
