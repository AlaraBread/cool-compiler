module Tac where

import Control.Monad.State
import Data.Int (Int32)
import qualified InputIr

type Tac = [TacStatement]

data TacStatement
  = Add Variable Variable Variable
  | Subtract Variable Variable Variable
  | Multiply Variable Variable Variable
  | Divide Variable Variable Variable
  | LessThan Variable Variable Variable
  | LessThanOrEqualTo Variable Variable Variable
  | Equals Variable Variable Variable
  | IntConstant Int32
  | BoolConstant Bool
  | StringConstant String
  | Not Variable Variable
  | Negate Variable Variable
  | New Variable InputIr.Type
  | Default Variable InputIr.Type
  | IsVoid Variable Variable
  | Call Variable [Variable]
  | Jump Label
  | TacLabel Label
  | Return Variable
  | Comment String
  | ConditionalJump Variable Label
  | Assign Variable Variable

data Variable = StringV String | TemporaryV Temporary

newtype Label = Label String

newtype Temporary = Temporary Int

getVariable :: State Temporary Variable
getVariable = state $ \(Temporary i) -> (TemporaryV $ Temporary $ i + 1, Temporary $ i + 1)

getLabel :: State Temporary Label
getLabel = state $ \(Temporary i) -> (Label $ "l" ++ show (i + 1), Temporary $ i + 1)

generateTac :: InputIr.Typed InputIr.Expr -> State Temporary (Tac, Variable)
generateTac
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
          (tac, variable) <- generateTac exp
          let lhs = StringV lexeme
          return (tac ++ [Assign lhs variable], lhs)
    InputIr.DynamicDispatch
      { InputIr.dynamicDispatchLhs = lhs,
        InputIr.dynamicDispatchMethod = method,
        InputIr.dynamicDispachArgs = args
      } -> undefined
    InputIr.StaticDispatch
      { InputIr.staticDispatchLhs,
        InputIr.dynamicDispatchType,
        InputIr.dynamicDispatchMethod,
        InputIr.dynamicDispatchArgs
      } -> undefined
    InputIr.SelfDispatch
      { InputIr.selfDispatchMethod,
        InputIr.selfDispatchArgs
      } -> undefined
    InputIr.If
      { InputIr.ifPredicate,
        InputIr.trueBody,
        InputIr.falseBody
      } -> do
        (predicateTac, predicateV) <- generateTac ifPredicate
        (trueTac, trueV) <- generateTac trueBody
        (falseTac, falseV) <- generateTac falseBody
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
        (predicateTac, predicateV) <- generateTac whilePredicate
        (bodyTac, bodyV) <- generateTac whileBody
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
      expressions <- traverse generateTac expressions
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
                        (rhsTac, rhsV) <- generateTac rhs
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
      (bodyTac, bodyV) <- generateTac body
      pure (bindingInitTac ++ bodyTac ++ bindingResetTac, bodyV)
    InputIr.Case e elements -> undefined

binaryOperation ::
  InputIr.Typed InputIr.Expr ->
  InputIr.Typed InputIr.Expr ->
  (Variable -> Variable -> Variable -> TacStatement) ->
  State Temporary (Tac, Variable)
binaryOperation a b op = do
  (aTac, aV) <- generateTac a
  (bTac, bV) <- generateTac b
  resultV <- getVariable
  pure ([op resultV aV bV], resultV)

unaryOperation ::
  InputIr.Typed InputIr.Expr ->
  (Variable -> Variable -> TacStatement) ->
  State Temporary (Tac, Variable)
unaryOperation exp op = do
  (tac, v) <- generateTac exp
  resultV <- getVariable
  pure ([op resultV v], resultV)

constant :: (a -> TacStatement) -> a -> State Temporary (Tac, Variable)
constant statement c = do
  resultV <- getVariable
  pure ([statement c], resultV)
