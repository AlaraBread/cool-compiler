module Tac where

import Control.Monad.State
import Data.Int (Int32)
import InputIr (ExprWithoutLine (Negate))
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
          return (Assign lhs variable : tac, lhs)
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
      } -> undefined
    InputIr.While
      { InputIr.whilePredicate,
        InputIr.whileBody
      } -> undefined
    InputIr.Block expressions -> undefined
    InputIr.New identifier -> undefined
    InputIr.IsVoid exp -> undefined
    InputIr.Plus a b -> undefined
    InputIr.Minus a b -> undefined
    InputIr.Times a b -> undefined
    InputIr.Divide a b -> undefined
    InputIr.LessThan a b -> undefined
    InputIr.LessThanOrEqualTo a b -> undefined
    InputIr.Equal a b -> undefined
    InputIr.Not e -> undefined
    InputIr.Negate e -> undefined
    InputIr.IntegerConstant i -> undefined
    InputIr.StringConstant s -> undefined
    InputIr.Variable identifier -> undefined
    InputIr.BooleanConstant b -> undefined
    InputIr.Let bindings body -> undefined
    InputIr.Case e elements -> undefined
