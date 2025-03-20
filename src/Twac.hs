import Data.Int (Int32)
import qualified Data.Map as Map
import InputIr (Formal, Type)
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

data TwacIr = TwacIr {implementationMap :: Map.Map InputIr.Type [TwacMethod], constructorMap :: Map.Map InputIr.Type Twac}

data TwacMethod = TwacMethod {methodName :: String, body :: Twac, formals :: [Formal], returnVariable :: Variable}
