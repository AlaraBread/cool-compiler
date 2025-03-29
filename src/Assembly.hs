import Control.Monad.State
import Data.Maybe (maybe)
import qualified Data.Map.Strict as Map
import Distribution.Compat.CharParsing (CharParsing (string))
import qualified InputIr
import Trac (Label (..), Temporary (Temporary), TypeDetailsMap, Variable (AttributeV, ParameterV, TemporaryV), getVariable)
import qualified Twac
import qualified TwacR

data AssemblyStatement
  = Add TwacR.Register TwacR.Register
  | AddImmediate Int TwacR.Register
  | Subtract TwacR.Register TwacR.Register
  | SubtractImmediate Int TwacR.Register
  | Multiply TwacR.Register TwacR.Register
  | Divide TwacR.Register
  | Cqto
  | Cmp TwacR.Register TwacR.Register
  | Test TwacR.Register TwacR.Register
  | TestConst TwacR.Register Integer
  | Store TwacR.Register Address
  | Load Address TwacR.Register
  | LoadConst Integer TwacR.Register
  | Transfer TwacR.Register TwacR.Register
  | Push TwacR.Register
  | Pop TwacR.Register
  | Not TwacR.Register
  | Negate TwacR.Register
  | AssemblyLabel Label
  | Call Label
  | DynamicCall TwacR.Register
  | Return
  | Jump Label
  | JumpZero Label
  | JumpNonZero Label
  | JumpLessThan Label
  | JumpLessThanEqual Label
  | LoadLabel Label TwacR.Register
  | JumpAddress Address
  | Comment String

instance Show AssemblyStatement where
  show instruction =
    let indent = (++) "    " 
        unary op arg = indent $ op ++ " " ++ show arg
        binary op src dst = indent $ op ++ " " ++ show src ++ " " ++ show dst
    
    in case instruction of
      Add src dst -> binary "addq" src dst
      AddImmediate src dst -> binary "addq" src dst
      Subtract src dst -> binary "subq" src dst
      SubtractImmediate src dst -> binary "subq" src dst
      Multiply src dst -> binary "imulq" src dst
      Divide src -> unary "idivq" src
      Cqto -> "cqto"
      Cmp src dst -> binary "cmpq" src dst
      Test src dst -> binary "testq" src dst
      TestConst src dst -> binary "testq" src dst
      Store src dst -> binary "movq" src dst
      Load src dst -> binary "movq" src dst
      LoadConst src dst -> binary "movq" src dst
      Transfer src dst -> binary "movq" src dst
      Push src -> unary "pushq" src
      Pop dst -> unary "pushq" dst
      Not dst -> unary "notq" dst
      Negate dst -> unary "negl" dst
      AssemblyLabel (Label label) -> label ++ ":"
      Call label -> unary "call" label
      DynamicCall label -> unary "call" label
      Return -> indent "ret"
      Jump label -> unary "jmp" label
      JumpZero label -> unary "jz" label
      JumpNonZero label -> unary "jnz" label
      JumpLessThan label -> unary "jl" label
      JumpLessThanEqual label -> unary "jle" label
      LoadLabel label dst -> binary "movq" label dst
      JumpAddress addr -> unary "jmp" addr
      Comment string -> indent $ "## " ++ string

data AssemblyData
  = JumpTable Label [Label]
  | StringConstant Label String
  | VTable
      { vTableTypeName :: Label,
        vTableTypeId :: Integer,
        vTableMethods :: [Label]
      }

data Address = Address
  { addressOffset :: Maybe Int,
    addressBase :: TwacR.Register,
    addressIndex :: Maybe TwacR.Register,
    addressScale :: Maybe Int
  }

instance Show Address where
  show (Address offset base index scale) = 
    maybe "" show offset
    ++ "("
    ++ show base 
    ++ ","
    ++ maybe "" show index
    ++ ","
    ++ maybe "" show scale
    ++ ")"

data AssemblyIr = AssemblyIr
  { assemblyIrCode :: [AssemblyStatement],
    assemblyIrData :: [AssemblyData]
  }

generateAssembly :: TwacR.TwacRIr -> AssemblyIr
generateAssembly TwacR.TwacRIr {TwacR.implementationMap, TwacR.constructorMap} =
  AssemblyIr
    { assemblyIrCode = concat $ Map.mapWithKey (concatMap . generateAssemblyMethod) implementationMap,
      assemblyIrData = undefined
    }

generateAssemblyMethod :: InputIr.Type -> TwacR.TwacRMethod -> [AssemblyStatement]
generateAssemblyMethod = undefined

-- Let n be the number of arguments.
-- Let r be the number of register arguments.
-- Let t be the number of temporaries.

-- Stack Layout
--   8(n-6)+16(%rbp) | argument n
--                   | ...
--          24(%rbp) | argument 7
--          16(%rbp) | argument 6
--                   | --------------------
--           8(%rbp) | saved return address
--            (%rbp) | saved %rbp
--          -8(%rbp) | saved %rbx
--         -16(%rbp) | saved %rsp
--         -24(%rbp) | saved %r12
--         -32(%rbp) | saved %r13
--         -40(%rbp) | saved %r14
--         -48(%rbp) | saved %r15
--         -56(%rbp) | argument 0
--                   | ...
--      -8r-56(%rbp) | argument r
--      -8r-64(%rbp) | temporary 0
--                   | ...
--  -8(r+t)-64(%rbp) | temporary t

getAddress :: Int -> Variable -> Address
getAddress registerParamCount variable = case variable of
  TemporaryV t ->
    Address (Just $ -8 * (registerParamCount + t) - 64) TwacR.Rbp Nothing Nothing
  AttributeV n ->
    Address (Just $ (3 + n) * 8) TwacR.R15 Nothing Nothing
  ParameterV n ->
    if n < 6
      then Address (Just $ -8 * n - 56) TwacR.Rbp Nothing Nothing
      else Address (Just $ 8 * (n - 6) + 16) TwacR.Rbp Nothing Nothing

-- Gives the address of the nth attribute pointed to by the given register
attributeAddress :: TwacR.Register -> Int -> Address
attributeAddress reg n = Address (Just n) reg Nothing Nothing

generateAssemblyStatements :: Int -> TypeDetailsMap -> TwacR.TwacRStatement -> ([AssemblyStatement], [AssemblyData])
generateAssemblyStatements registerParamCount typeDetailsMap twacRStatement =
  let getAddress' = getAddress registerParamCount
      instOnly x = (x, [])
      -- scratch registers
      r1 = TwacR.R10
      r2 = TwacR.R11
   in case twacRStatement of
        TwacR.Load src dst -> instOnly [Load (getAddress' src) dst]
        TwacR.Store src dst -> instOnly [Store src (getAddress' dst)]
        TwacR.Push src -> instOnly [Push src]
        TwacR.Pop dst -> instOnly [Pop dst]
        TwacR.AllocateStackSpace words -> instOnly [AddImmediate (words * 8) TwacR.Rsp]
        TwacR.DeallocateStackSpace words -> instOnly [SubtractImmediate (words * 8) TwacR.Rsp]
        TwacR.TwacRStatement twacStatement -> case twacStatement of
          Twac.Add src dst ->
            instOnly
              [ Load (attributeAddress src 0) r1,
                Load (attributeAddress dst 0) r2,
                Add r1 r2,
                Store r2 (attributeAddress dst 0)
              ]
          Twac.Subtract src dst ->
            instOnly
              [ Load (attributeAddress src 0) r1,
                Load (attributeAddress dst 0) r2,
                Subtract r1 r2,
                Store r2 (attributeAddress dst 0)
              ]
          Twac.Multiply src dst ->
            instOnly
              [ Load (attributeAddress src 0) r1,
                Load (attributeAddress dst 0) r2,
                Multiply r1 r2,
                Store r2 (attributeAddress dst 0)
              ]
          Twac.Divide src dst ->
            instOnly
              [ Cqto,
                Divide src,
                Store TwacR.Rax (attributeAddress dst 0)
              ]
          Twac.LessThan src dst -> undefined
          Twac.LessThanOrEqualTo src dst -> undefined
          Twac.Equals src dst -> undefined
          Twac.IntConstant i dst -> undefined
          Twac.BoolConstant i dst -> undefined
          Twac.StringConstant i dst -> undefined
          Twac.Not dst -> undefined
          Twac.Negate dst -> undefined
          Twac.New type' dst -> undefined
          Twac.Default type' dst -> undefined
          Twac.IsVoid dst -> undefined
          Twac.Dispatch dispatchResult dispatchReceiver dispatchReceiverType dispatchType dispatchMethod dispatchArgs -> undefined
          Twac.Jump label -> instOnly [Jump label]
          Twac.TwacLabel label -> instOnly [AssemblyLabel label]
          Twac.Return _ -> instOnly [Return]
          Twac.Comment string -> instOnly [Comment string]
          Twac.ConditionalJump cond label -> undefined
          Twac.Assign src dst -> instOnly [Transfer src dst]
          Twac.TwacCase condition jmpTable -> undefined
          Twac.Abort line string -> undefined
