import Data.Int (Int32)
import InputIr (Type)
import Trac (Label, Variable)

type Twac = [TwacStatement]

data TwacStatement
  = Add Variable Variable
  | Subtract Variable Variable
  | Multiply Variable Variable
  | Divide Variable Variable
  | LessThan Variable Variable
  | LessThanOrEqualTo Variable Variable
  | Equals Variable Variable
  | IntConstant Variable Int32
  | BoolConstant Variable Bool
  | StringConstant Variable String
  | Not Variable
  | Negate Variable
  | New Variable Type
  | Default Variable Type
  | IsVoid Variable
  | Dispatch
      { dispatchResult :: Variable,
        dispatchReceiver :: Variable,
        dispatchType :: Maybe Type,
        dispatchMethod :: String,
        dispatchArgs :: [Variable]
      }
  | Jump Label
  | TacLabel Label
  | Return Variable
  | Comment String
  | ConditionalJump Variable Label
  | Assign Variable Variable
  | TwacCase Variable [TwacCaseElement]

data TwacCaseElement = TwacCaseElement [InputIr.Type] (Twac, Variable)
