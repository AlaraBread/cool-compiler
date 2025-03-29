module Assembly where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Maybe (maybe)
import Distribution.Compat.CharParsing (CharParsing (string))
import qualified InputIr
import Trac (Label (..), Temporary (Temporary), TypeDetails (TypeDetails, typeSize), TypeDetailsMap, Variable (AttributeV, ParameterV, TemporaryV), getLabel, getVariable)
import qualified Twac
import TwacR (TwacRStatement (TwacRStatement))
import qualified TwacR
import Util

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
  | StoreConst Int Address
  | Load Address TwacR.Register
  | LoadConst Int TwacR.Register
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
          StoreConst src dst -> binary "movq" src dst
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

instance Show AssemblyIr where
  show (AssemblyIr code data') = unlines (map show code)

-- TODO: actually generate constructors... 
generateAssembly :: Temporary -> TwacR.TwacRIr -> AssemblyIr
generateAssembly temporaryState TwacR.TwacRIr {TwacR.implementationMap, TwacR.constructorMap, TwacR.typeDetailsMap} = evalState
    (do
      let methodList = map snd $ Map.toList implementationMap
      x <- traverse (traverse $ generateAssemblyMethod typeDetailsMap) methodList
      let (code, data') = traverse combineAssembly x
      pure $ AssemblyIr code (concat data')
    )
    temporaryState

generateAssemblyMethod :: TypeDetailsMap -> TwacR.TwacRMethod -> State Temporary ([AssemblyStatement], [AssemblyData])
generateAssemblyMethod typeDetailsMap method = do
  lines <- traverse
    (generateAssemblyStatements (TwacR.registerParamCount method) typeDetailsMap . item)
    (TwacR.body method)
  pure $ combineAssembly lines

combineAssembly :: [([AssemblyStatement], [AssemblyData])] -> ([AssemblyStatement], [AssemblyData])
combineAssembly asm = let
    statements = concatMap fst asm
    data' = concatMap snd asm
  in (statements, data')

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

generateAssemblyStatements :: Int -> TypeDetailsMap -> TwacR.TwacRStatement -> State Temporary ([AssemblyStatement], [AssemblyData])
generateAssemblyStatements registerParamCount typeDetailsMap twacRStatement =
  let generateAssemblyStatements' = generateAssemblyStatements registerParamCount typeDetailsMap
      getAddress' = getAddress registerParamCount
      instOnly x = (x, [])
      -- scratch registers
      r1 = TwacR.R10
      r2 = TwacR.R11
   in case twacRStatement of
        TwacR.Load src dst ->
          pure $ instOnly [Load (getAddress' src) dst]
        TwacR.Store src dst ->
          pure $ instOnly [Store src (getAddress' dst)]
        TwacR.Push src ->
          pure $ instOnly [Push src]
        TwacR.Pop dst ->
          pure $ instOnly [Pop dst]
        TwacR.AllocateStackSpace words ->
          pure $ instOnly [AddImmediate (words * 8) TwacR.Rsp]
        TwacR.DeallocateStackSpace words ->
          pure $ instOnly [SubtractImmediate (words * 8) TwacR.Rsp]
        TwacR.TwacRStatement twacStatement -> case twacStatement of
          Twac.Add src dst ->
            pure $
              instOnly
                [ Load (attributeAddress src 0) r1,
                  Load (attributeAddress dst 0) r2,
                  Add r1 r2,
                  Store r2 (attributeAddress dst 0)
                ]
          Twac.Subtract src dst ->
            pure $
              instOnly
                [ Load (attributeAddress src 0) r1,
                  Load (attributeAddress dst 0) r2,
                  Subtract r1 r2,
                  Store r2 (attributeAddress dst 0)
                ]
          Twac.Multiply src dst ->
            pure $
              instOnly
                [ Load (attributeAddress src 0) r1,
                  Load (attributeAddress dst 0) r2,
                  Multiply r1 r2,
                  Store r2 (attributeAddress dst 0)
                ]
          Twac.Divide src dst ->
            pure $
              instOnly
                [ Cqto,
                  Divide src,
                  Store TwacR.Rax (attributeAddress dst 0)
                ]
          Twac.LessThan src dst -> do
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Bool") r1
            setting <- setBool r1 JumpLessThan
            pure $ instOnly $ construction ++ [Cmp src dst] ++ setting ++ [Transfer r1 dst]
          Twac.LessThanOrEqualTo src dst -> do
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Bool") r1
            setting <- setBool r1 JumpLessThanEqual
            pure $ instOnly $ construction ++ [Cmp src dst] ++ setting ++ [Transfer r1 dst]
          Twac.Equals src dst -> do
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Bool") r1
            setting <- setBool r1 JumpZero
            pure $ instOnly $ construction ++ [Cmp src dst] ++ setting ++ [Transfer r1 dst]
          Twac.IntConstant i dst -> do
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Int") dst
            pure $ instOnly $ construction ++ [LoadConst i r1, Store r1 (attributeAddress dst 0)]
          Twac.BoolConstant b dst -> do
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Int") dst
            pure $ instOnly $ construction ++ [LoadConst (if b then -1 else 0) r1, Store r1 (attributeAddress dst 0)]
          Twac.StringConstant s dst -> undefined
          Twac.Not dst ->
            pure $ instOnly [Not dst]
          Twac.Negate dst ->
            pure $ instOnly [Negate dst]
          -- TODO: set vtable pointer
          -- TODO: deal with SELF_TYPE, somehow (look at type tag of self?)
          Twac.New type' dst ->
            let (TypeDetails tag size) = typeDetailsMap Map.! type'
             in pure $
                  instOnly $
                    calloc size
                      ++ [ Transfer TwacR.Rax dst,
                           -- accessing negative attributes is a cursed, but
                           -- correct way of doing this.
                           StoreConst size (attributeAddress dst $ -3),
                           StoreConst tag (attributeAddress dst $ -2)
                         ]
          Twac.Default type' dst ->
            let delegateToNew = generateAssemblyStatements' $ TwacRStatement $ Twac.New type' dst
             in case type' of
                  -- Int, Bool, String all have special default handling.
                  -- However, this special handling happens to do the exact same
                  -- thing as New. So, let us just generate assembly for that,
                  -- so we do not have to do the fiddly bit twice :).
                  InputIr.Type "Int" -> delegateToNew
                  InputIr.Type "Bool" -> delegateToNew
                  InputIr.Type "String" -> delegateToNew
                  typeOther -> pure $ instOnly [LoadConst 0 dst]
          Twac.IsVoid dst -> undefined
          -- TODO: make this not *incredibly* janky, lol
          Twac.Dispatch dispatchResult dispatchReceiver dispatchReceiverType dispatchType dispatchMethod dispatchArgs -> undefined
          Twac.Jump label ->
            pure $ instOnly [Jump label]
          Twac.TwacLabel label ->
            pure $ instOnly [AssemblyLabel label]
          Twac.Return _ ->
            pure $ instOnly [Return]
          Twac.Comment string ->
            pure $ instOnly [Comment string]
          Twac.ConditionalJump cond label ->
            pure $
              instOnly
                [ Load (attributeAddress cond 0) r1,
                  Test r1 r1,
                  JumpNonZero label
                ]
          Twac.Assign src dst ->
            pure $ instOnly [Transfer src dst]
          Twac.TwacCase condition jmpTable -> undefined
          Twac.Abort line string -> undefined

-- Inspired by the reference compiler output, though there really is just about
-- one way to do this. Using calloc instead of malloc means we can save the
-- multiply/bit shift for libc instead of doing it ourselves, and that makes us
-- happy.
calloc :: Int -> [AssemblyStatement]
calloc words =
  [ LoadConst 8 TwacR.Rsi,
    LoadConst words TwacR.Rdi,
    Call $ Label "calloc"
  ]

-- Sets a boolean based on a conditional jump passed in. If the conditional jump
-- occurs, we set the boolean to true; otherwise, we leave it as false. Note, we
-- spill %r11.
setBool :: TwacR.Register -> (Label -> AssemblyStatement) -> State Temporary [AssemblyStatement]
setBool boolReg conditionalJump = do
  trueLabel <- getLabel
  falseLabel <- getLabel
  pure
    [ conditionalJump trueLabel,
      Jump falseLabel,
      AssemblyLabel trueLabel,
      LoadConst (-1) TwacR.R11,
      Store TwacR.R11 (attributeAddress boolReg 0),
      AssemblyLabel falseLabel
    ]
