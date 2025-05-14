{-# LANGUAGE NamedFieldPuns #-}

module TracIr where

import Data.Int (Int32)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Simple.Utils (lowercase)
import qualified InputIr
import Util

data TracIr v = TracIr
  { implementationMap :: Map.Map Type [InputIr.ImplementationMapEntry (TracMethod v)],
    typeDetailsMap :: TypeDetailsMap
  }
  deriving (Show)

data TracMethod v = TracMethod {methodName :: String, body :: Trac v, formals :: [InputIr.Formal], temporaryCount :: Int}
  deriving (Show)

type Trac v = [Lined (TracStatement v)]

data TracStatement v
  = Add v v v
  | Subtract v v v
  | Multiply v v v
  | Divide v v v
  | LessThan v v v
  | LessThanOrEqualTo v v v
  | Equals v v v
  | IntConstant v Int32
  | BoolConstant v Bool
  | StringConstant v String
  | Not v v
  | Negate v v
  | New v Type
  | Default v Type
  | IsVoid v v
  | Dispatch
      { dispatchResult :: v,
        dispatchReceiver :: v,
        dispatchReceiverType :: Type,
        dispatchType :: Maybe Type,
        dispatchMethod :: String,
        dispatchArgs :: [v]
      }
  | Jump Label
  | TracLabel Label
  | Return v
  | Comment String
  | ConditionalJump v Label Label
  | Assign v v
  | -- The map *must* cover every possible type, as later this gets lowered to a jump table
    -- dst, src, jumptable
    Case v v (Map.Map Type Label) Label
  | TracInternal InputIr.Internal
  | Abort Int (AbortReason v)
  | Phi v (Set.Set v)

data AbortReason v
  = DispatchOnVoid
  | StaticDispatchOnVoid
  | CaseOnVoid
  | CaseNoMatch v
  | DivisionByZero
  | SubstringOutOfRange
  deriving (Show)

instance (Show v) => Show (TracStatement v) where
  show statement = case statement of
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
    New a (Type t) -> show a ++ " <- new " ++ t
    Default a (Type t) -> show a ++ " <- default " ++ t
    IsVoid a b -> showUnary a b "isvoid"
    Dispatch
      { dispatchResult,
        dispatchMethod,
        dispatchArgs
      } -> show dispatchResult ++ " <- call " ++ dispatchMethod ++ " " ++ unwords (map show dispatchArgs)
    Jump l -> "jmp " ++ show l
    TracLabel l -> "label " ++ show l
    Return a -> "return " ++ show a
    Comment msg -> "comment " ++ msg
    ConditionalJump a l _ -> "bt " ++ show a ++ " " ++ show l
    Assign a b -> show a ++ " <- " ++ show b
    Case dst src labels _ -> show dst ++ " <- case " ++ show src ++ " " ++ show labels
    TracInternal internal -> "internal: " ++ show internal
    Abort line reason -> "abort " ++ show line ++ " " ++ show reason

showBinary :: (Show v) => v -> v -> v -> String -> String
showBinary a b c op = show a ++ " <- " ++ op ++ " " ++ show b ++ " " ++ show c

showUnary :: (Show v) => v -> v -> String -> String
showUnary a b op = show a ++ " <- " ++ op ++ " " ++ show b
