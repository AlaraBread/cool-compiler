module Tac where

import Control.Monad.State
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Distribution.Simple.Utils (intercalate, lowercase)
import qualified InputIr

data TacIr = TacIr {implementationMap :: Map.Map InputIr.Type [TacMethod], constructorMap :: Map.Map InputIr.Type Tac}
  deriving (Show)

data TacMethod = TacMethod {body :: Tac, formals :: [InputIr.Formal], returnVariable :: Variable}
  deriving (Show)

type Tac = [TacStatement]

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
        dispatchReciever :: Variable,
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
  show :: TacStatement -> String
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
        dispatchReciever,
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
showBinary a b c op = show a ++ " <- " ++ op ++ show b ++ show c

showUnary :: Variable -> Variable -> String -> String
showUnary a b op = show a ++ " <- " ++ op ++ show b

data Variable = StringV String | TemporaryV Temporary

instance Show Variable where
  show :: Variable -> String
  show (StringV s) = s
  show (TemporaryV (Temporary t)) = "temp" ++ show t

newtype Label = Label String

instance Show Label where
  show :: Label -> String
  show (Label l) = l

newtype Temporary = Temporary Int
  deriving (Show)

showTac :: Tac -> String
showTac tac = intercalate "\n" (map show tac)

getVariable :: State Temporary Variable
getVariable = state $ \(Temporary i) -> (TemporaryV $ Temporary $ i + 1, Temporary $ i + 1)

getLabel :: State Temporary Label
getLabel = state $ \(Temporary i) -> (Label $ "l" ++ show (i + 1), Temporary $ i + 1)

generateTacExpr :: InputIr.Typed InputIr.Expr -> State Temporary (Tac, Variable)
generateTacExpr
  InputIr.Typed
    { InputIr.type',
      InputIr.item = InputIr.Expr line_number expr
    } = case expr of
    InputIr.Assign
      InputIr.Identifier
        { InputIr.lexeme
        }
      exp ->
        do
          (tac, variable) <- generateTacExpr exp
          let lhs = StringV lexeme
          return (tac ++ [Assign lhs variable], lhs)
    InputIr.DynamicDispatch
      { InputIr.dynamicDispatchLhs = reciever,
        InputIr.dynamicDispatchMethod = method,
        InputIr.dynamicDispachArgs = args
      } -> do
        temp <- getVariable
        dispatchArgs' <- mapM generateTacExpr args
        let dispatchArgsTac = concatMap fst dispatchArgs'
        let dispatchArgsV = map snd dispatchArgs'
        (recieverTac, recieverV) <- generateTacExpr reciever
        pure
          ( dispatchArgsTac
              ++ recieverTac
              ++ [ Dispatch
                     { dispatchResult = temp,
                       dispatchMethod = InputIr.lexeme method,
                       dispatchReciever = recieverV,
                       dispatchType = Nothing,
                       dispatchArgs = dispatchArgsV
                     }
                 ],
            temp
          )
    InputIr.StaticDispatch
      { InputIr.staticDispatchLhs = reciever,
        InputIr.dynamicDispatchType = type',
        InputIr.dynamicDispatchMethod = method,
        InputIr.dynamicDispatchArgs = args
      } -> do
        temp <- getVariable
        dispatchArgs' <- mapM generateTacExpr args
        let dispatchArgsTac = concatMap fst dispatchArgs'
        let dispatchArgsV = map snd dispatchArgs'
        (recieverTac, recieverV) <- generateTacExpr reciever
        pure
          ( dispatchArgsTac
              ++ recieverTac
              ++ [ Dispatch
                     { dispatchResult = temp,
                       dispatchMethod = InputIr.lexeme method,
                       dispatchReciever = recieverV,
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
              ++ [ Dispatch
                     { dispatchResult = temp,
                       dispatchMethod = InputIr.lexeme selfDispatchMethod,
                       dispatchReciever = StringV "self",
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
        let trueTac = (TacLabel trueLabel : trueTac) ++ [Assign bodyV trueV, TacLabel trueEndLabel]
        let falseTac = falseTac ++ [Assign bodyV falseV, Jump trueEndLabel]
        return
          ( predicateTac
              ++ [ConditionalJump predicateV trueLabel]
              ++ falseTac
              ++ trueTac,
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
          ( [TacLabel whileStart]
              ++ predicateTac
              ++ [Not notPredicateV predicateV]
              ++ [ConditionalJump notPredicateV endLabel]
              ++ bodyTac
              ++ [Jump whileStart]
              ++ [TacLabel endLabel]
              ++ [Default outV $ InputIr.Type "Object"],
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
      pure ([New t $ InputIr.Type typeName], t)
    InputIr.IsVoid exp -> unaryOperation exp IsVoid
    InputIr.Plus a b -> binaryOperation a b Add
    InputIr.Minus a b -> binaryOperation a b Subtract
    InputIr.Times a b -> binaryOperation a b Multiply
    InputIr.Divide a b -> binaryOperation a b Divide
    InputIr.LessThan a b -> binaryOperation a b LessThan
    InputIr.LessThanOrEqualTo a b -> binaryOperation a b LessThanOrEqualTo
    InputIr.Equal a b -> binaryOperation a b Equals
    InputIr.Not exp -> unaryOperation exp Not
    InputIr.Negate exp -> unaryOperation exp Negate
    InputIr.IntegerConstant i -> constant IntConstant i
    InputIr.StringConstant s -> constant StringConstant s
    InputIr.BooleanConstant b -> constant BoolConstant b
    InputIr.Variable InputIr.Identifier {InputIr.lexeme} -> pure ([], StringV lexeme)
    InputIr.Let bindings body -> do
      -- we need to restore the old values of bindings after the let statement
      bindings <-
        traverse
          ( \InputIr.LetBinding
               { InputIr.letBindingName = InputIr.Identifier {InputIr.lexeme = name},
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
                              ++ [ Assign bindingV rhsV,
                                   Assign tmp rhsV
                                 ],
                            [Assign bindingV tmp]
                          )
                      Nothing -> do
                        tmp <- getVariable
                        pure
                          ( [ Default bindingV (InputIr.Type type'),
                              Default tmp (InputIr.Type type')
                            ],
                            [Assign bindingV tmp]
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

binaryOperation ::
  InputIr.Typed InputIr.Expr ->
  InputIr.Typed InputIr.Expr ->
  (Variable -> Variable -> Variable -> TacStatement) ->
  State Temporary (Tac, Variable)
binaryOperation a b op = do
  (aTac, aV) <- generateTacExpr a
  (bTac, bV) <- generateTacExpr b
  resultV <- getVariable
  pure ([op resultV aV bV], resultV)

unaryOperation ::
  InputIr.Typed InputIr.Expr ->
  (Variable -> Variable -> TacStatement) ->
  State Temporary (Tac, Variable)
unaryOperation exp op = do
  (tac, v) <- generateTacExpr exp
  resultV <- getVariable
  pure ([op resultV v], resultV)

constant :: (Variable -> a -> TacStatement) -> a -> State Temporary (Tac, Variable)
constant statement c = do
  resultV <- getVariable
  pure ([statement resultV c], resultV)

generateTacMethod :: InputIr.Method -> State Temporary TacMethod
generateTacMethod (InputIr.Method {InputIr.methodFormals, InputIr.methodBody}) = do
  (tac, v) <- generateTacExpr methodBody
  pure TacMethod {body = tac, formals = methodFormals, returnVariable = v}

generateTacConstructor :: [InputIr.Attribute] -> State Temporary Tac
generateTacConstructor attrs = do
  attrs' <-
    traverse
      ( \InputIr.Attribute
           { InputIr.attrName = InputIr.Identifier {InputIr.lexeme},
             InputIr.attrType,
             InputIr.attrRhs
           } -> case attrRhs of
            Just e -> do
              (tac, v) <- generateTacExpr e
              pure [Assign (StringV lexeme) v]
            Nothing -> pure [Default (StringV lexeme) attrType]
      )
      attrs
  pure $ concat attrs'

generateTac :: InputIr.InputIr -> TacIr
generateTac (InputIr.InputIr classMap implMap parentMap ast) =
  evalState
    ( do
        implMap' <- mapM (mapM generateTacMethod) implMap
        constructorMap <- mapM generateTacConstructor classMap
        pure TacIr {implementationMap = implMap', constructorMap = constructorMap}
    )
    ( Temporary
        0
    )
