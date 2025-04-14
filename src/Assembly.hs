{-# LANGUAGE NamedFieldPuns #-}

module Assembly where

import Control.Monad.State
import Data.Char (ord)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybe)
import Distribution.Compat.CharParsing (CharParsing (string))
import InputIr (ImplementationMapEntry)
import qualified InputIr
import Trac (Label (..), Temporary (Temporary), TracStatement (dispatchReceiverType), TypeDetails (TypeDetails, methodTags, typeSize, typeTag), TypeDetailsMap, Variable (AttributeV, ParameterV, TemporaryV), getLabel, getVariable)
import qualified Trac
import qualified Twac
import TwacR (TwacRStatement (TwacRStatement), showByte)
import qualified TwacR
import Util

data AssemblyStatement
  = Add TwacR.Register TwacR.Register
  | Add64 TwacR.Register TwacR.Register
  | AddImmediate Int TwacR.Register
  | AddImmediate64 Int TwacR.Register
  | Subtract TwacR.Register TwacR.Register
  | SubtractImmediate Int TwacR.Register
  | SubtractImmediate64 Int TwacR.Register
  | Multiply TwacR.Register TwacR.Register
  | MultiplyConst Int TwacR.Register
  | Divide TwacR.Register
  | Cdq
  | Cmp TwacR.Register TwacR.Register
  | Cmp64 TwacR.Register TwacR.Register
  | Test TwacR.Register TwacR.Register
  | Test64 TwacR.Register TwacR.Register
  | CmpConst Integer TwacR.Register
  | CmpConst32 Integer TwacR.Register
  | XorConst Integer TwacR.Register
  | Store TwacR.Register Address
  | StoreByte TwacR.Register Address
  | StoreConst Int Address
  | Lea Address TwacR.Register
  | Load Address TwacR.Register
  | LoadByte Address TwacR.Register
  | LoadConst Int TwacR.Register
  | Transfer TwacR.Register TwacR.Register
  | TransferConst Int TwacR.Register
  | Push TwacR.Register
  | Pop TwacR.Register
  | Not TwacR.Register
  | Negate TwacR.Register
  | AssemblyLabel Label
  | Global Label
  | Call Label
  | CallAddress Address
  | Return
  | Jump Label
  | JumpZero Label
  | JumpNonZero Label
  | JumpLessThan Label
  | JumpLessThanEqual Label
  | JumpGreaterThan Label
  | JumpGreaterThanEqual Label
  | LoadLabel Label TwacR.Register
  | JumpAddress Address
  | Comment String

instance Show AssemblyStatement where
  show instruction =
    let indent = (++) "    "
        unary op arg = indent $ op ++ " " ++ show arg
        unary32 op arg = indent $ op ++ " " ++ TwacR.show32 arg

        binary op src dst = indent $ op ++ " " ++ show src ++ ", " ++ show dst
        binary32 op src dst = indent $ op ++ " " ++ TwacR.show32 src ++ ", " ++ TwacR.show32 dst

        binaryConst op src dst = indent $ op ++ " $" ++ show src ++ ", " ++ show dst
        binaryConst32 op src dst = indent $ op ++ " $" ++ show src ++ ", " ++ TwacR.show32 dst
     in case instruction of
          Add src dst -> binary32 "addl" src dst
          Add64 src dst -> binary "addq" src dst
          AddImmediate src dst -> binaryConst32 "addl" src dst
          AddImmediate64 src dst -> binaryConst "addq" src dst
          Subtract src dst -> binary32 "subl" src dst
          SubtractImmediate src dst -> binaryConst32 "subl" src dst
          SubtractImmediate64 src dst -> binaryConst "subq" src dst
          Multiply src dst -> binary32 "imull" src dst
          MultiplyConst src dst -> binaryConst "imulq" src dst
          Divide src -> unary32 "idivl" src
          Cdq -> indent "cdq"
          Cmp src dst -> binary32 "cmpl" src dst
          Cmp64 src dst -> binary "cmpq" src dst
          Test src dst -> binary32 "testl" src dst
          Test64 src dst -> binary "test" src dst
          CmpConst src dst -> binaryConst "cmpq" src dst
          CmpConst32 src dst -> binaryConst32 "cmpl" src dst
          XorConst src dst -> binaryConst "xorq" src dst
          Store src dst -> binary "movq" src dst
          StoreByte src dst -> indent $ "mov " ++ showByte src ++ ", " ++ show dst
          StoreConst src dst -> binaryConst "movq" src dst
          Lea src dst -> binary "leaq" src dst
          Load src dst -> binary "movq" src dst
          LoadByte src dst -> indent $ "mov " ++ show src ++ ", " ++ showByte dst
          LoadConst src dst -> binaryConst "movq" src dst
          Transfer src dst -> binary "movq" src dst
          TransferConst src dst -> binaryConst "movq" src dst
          Push src -> unary "pushq" src
          Pop dst -> unary "popq" dst
          Not dst -> unary "notq" dst
          Negate dst -> unary32 "negl" dst
          AssemblyLabel (Label label) -> label ++ ":"
          Global (Label label) -> ".globl " ++ label
          Call label -> unary "call" label
          CallAddress addr -> indent $ "call *" ++ show addr
          Return -> indent "ret"
          Jump label -> unary "jmp" label
          JumpZero label -> unary "jz" label
          JumpNonZero label -> unary "jnz" label
          JumpLessThan label -> unary "jl" label
          JumpLessThanEqual label -> unary "jle" label
          JumpGreaterThan label -> unary "jg" label
          JumpGreaterThanEqual label -> unary "jge" label
          LoadLabel label dst -> binary "leaq" label dst
          JumpAddress addr -> indent $ "jmp *" ++ show addr
          Comment string -> indent $ "## " ++ string

-- TODO: reuse string constants if they are already past
data AssemblyData
  = JumpTable Label [Label]
  | TypeNameTable Label [Label]
  | -- This is the same as the physical string layout
    StringConstant
      { stringConstantLabel :: Label,
        stringConstantObjectSize :: Int,
        stringConstantTypeTag :: Int,
        stringConstantVTable :: Label,
        stringConstantSize :: Int,
        stringConstantCapacity :: Int,
        stringConstant :: String
      }
  | RawStringConstant Label String
  | VTable
      { vTableLabel :: Label,
        vTableMethods :: [Label]
      }

-- TODO: handle the other types of data (not needed for PA3c3)
instance Show AssemblyData where
  show data' = case data' of
    -- this is the exact same as VTables, lol
    JumpTable label entries -> show label ++ ":\n" ++ unlines (map ((++) "    .quad " . show) entries)
    TypeNameTable label entries -> show label ++ ":\n" ++ unlines (map ((++) "    .quad " . show) entries)
    StringConstant
      { stringConstantLabel,
        stringConstantObjectSize,
        stringConstantTypeTag,
        stringConstantVTable,
        stringConstantSize,
        stringConstantCapacity,
        stringConstant
      } ->
        let Label stringConstantLabelName = stringConstantLabel
            contentsLabel = Label $ stringConstantLabelName ++ "_contents"
         in (show stringConstantLabel ++ ":\n")
              ++ ("    .quad " ++ show stringConstantObjectSize ++ "\n")
              ++ ("    .quad " ++ show stringConstantTypeTag ++ "\n")
              ++ ("    .quad " ++ show stringConstantVTable ++ "\n")
              ++ ("    .quad " ++ show contentsLabel ++ "\n")
              ++ ("    .quad " ++ show stringConstantSize ++ "\n")
              ++ ("    .quad " ++ show stringConstantCapacity ++ "\n")
              ++ (show contentsLabel ++ ": .string \"" ++ stringConstant ++ "\"")
    RawStringConstant label text -> show label ++ ": .string \"" ++ text ++ "\""
    -- this is directly taken from the reference compiler
    VTable label entries -> show label ++ ":\n" ++ unlines (map ((++) "    .quad " . show) entries)

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
      ++ maybe "" (\i -> "," ++ show i) index
      ++ maybe "" (\i -> "," ++ show i) scale
      ++ ")"

data AssemblyIr = AssemblyIr
  { assemblyIrCode :: [AssemblyStatement],
    assemblyIrData :: [AssemblyData]
  }

instance Show AssemblyIr where
  show (AssemblyIr code data') =
    -- marks the stack as non-executable
    ".section .note.GNU-stack, \"\", @progbits\n\n"
      ++ ".section .data\n"
      ++ unlines (map show data')
      ++ "\n\n"
      ++ ".section .text\n"
      ++ unlines (map show code)

generateAssembly :: Temporary -> TwacR.TwacRIr -> AssemblyIr
generateAssembly temporaryState TwacR.TwacRIr {TwacR.implementationMap, TwacR.typeDetailsMap} =
  uncurry AssemblyIr $
    combineAssembly $
      evalState
        ( sequence
            [ pure $ combineAssembly $ generateVTable typeDetailsMap <$> Map.toList implementationMap,
              main' typeDetailsMap,
              lessThanOrEqualTo typeDetailsMap,
              equalTo typeDetailsMap,
              lessThan typeDetailsMap,
              pure $ inInt typeDetailsMap,
              pure outInt,
              pure outString,
              inString typeDetailsMap,
              pushString,
              concatString typeDetailsMap,
              stringLength typeDetailsMap,
              stringSubstr typeDetailsMap,
              objectCopy,
              typeName typeDetailsMap,
              abort,
              errorMessages,
              do
                let methodList =
                      fmap
                        (\(type', methods) -> (type', mapMaybe InputIr.implementationMapEntryToMaybe methods))
                        (Map.toList implementationMap)
                methods <- traverse (\(type', methods) -> traverse (generateAssemblyMethod type' typeDetailsMap) methods) methodList
                let (code, data') = traverse combineAssembly methods
                pure (code, concat data')
            ]
        )
        temporaryState

generateVTable :: TypeDetailsMap -> (InputIr.Type, [ImplementationMapEntry TwacR.TwacRMethod]) -> ([AssemblyStatement], [AssemblyData])
generateVTable typeDetailsMap (InputIr.Type selfType, entries) =
  let vTableMethod :: ImplementationMapEntry TwacR.TwacRMethod -> Label
      vTableMethod (InputIr.LocalImpl m) = Label $ selfType ++ "." ++ TwacR.methodName m
      vTableMethod (InputIr.ParentImpl (InputIr.Type parentType) m) = Label $ parentType ++ "." ++ m
   in ( [],
        [ VTable
            { vTableLabel = Label $ selfType ++ "..vtable",
              vTableMethods = map vTableMethod entries
            }
        ]
      )

generateAssemblyMethod :: InputIr.Type -> TypeDetailsMap -> TwacR.TwacRMethod -> State Temporary ([AssemblyStatement], [AssemblyData])
generateAssemblyMethod selfType typeDetailsMap method = do
  lines <-
    traverse
      (generateAssemblyStatements selfType (TwacR.registerParamCount method) typeDetailsMap . item)
      (TwacR.body method)

  pure $ combineAssembly lines

combineAssembly :: [([AssemblyStatement], [AssemblyData])] -> ([AssemblyStatement], [AssemblyData])
combineAssembly asm =
  let statements = concatMap fst asm
      data' = concatMap snd asm
   in (statements, data')

combineAssembly' :: [State Temporary ([AssemblyStatement], [AssemblyData])] -> State Temporary ([AssemblyStatement], [AssemblyData])
combineAssembly' asm = do
  a <- sequence asm
  let statements = concatMap fst a
  let data' = concatMap snd a
  pure (statements, data')

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
--         -16(%rbp) | saved %r12
--         -24(%rbp) | saved %r13
--         -32(%rbp) | saved %r14
--         -40(%rbp) | saved %r15
--         -48(%rbp) | argument 0
--                   | ...
--      -8r-48(%rbp) | argument r
--      -8r-56(%rbp) | temporary 0
--                   | ...
--  -8(r+t)-56(%rbp) | temporary t

getAddress :: Int -> Variable -> Address
getAddress registerParamCount variable = case variable of
  TemporaryV t ->
    Address (Just $ -8 * (registerParamCount + t) - 56) TwacR.Rbp Nothing Nothing
  AttributeV n ->
    attributeAddress TwacR.R15 n
  ParameterV n ->
    if n < 6
      then Address (Just $ -8 * n - 48) TwacR.Rbp Nothing Nothing
      else Address (Just $ 8 * (n - 6) + 16) TwacR.Rbp Nothing Nothing

-- Gives the base address of an object
baseAddress :: TwacR.Register -> Address
baseAddress r = Address Nothing r Nothing Nothing

-- Gives the size address of an object
sizeAddress :: TwacR.Register -> Address
sizeAddress r = Address (Just 0) r Nothing Nothing

-- Gives the type tag address of an object
typeTagAddress :: TwacR.Register -> Address
typeTagAddress r = Address (Just 8) r Nothing Nothing

-- Gives the address of the ptr to the vtable of an object
vTableAddress :: TwacR.Register -> Address
vTableAddress r = Address (Just 16) r Nothing Nothing

-- Gives the address of a ptr to the nth method in a vtable
vTableMethodAddress :: TwacR.Register -> Int -> Address
vTableMethodAddress r n = Address (Just $ n * 8) r Nothing Nothing

-- Gives the address of the nth attribute pointed to by the given register
attributeAddress :: TwacR.Register -> Int -> Address
attributeAddress reg n = Address (Just $ (n + 3) * 8) reg Nothing Nothing

generateAssemblyStatements :: InputIr.Type -> Int -> TypeDetailsMap -> TwacR.TwacRStatement -> State Temporary ([AssemblyStatement], [AssemblyData])
generateAssemblyStatements selfType registerParamCount typeDetailsMap twacRStatement =
  let generateAssemblyStatements' = generateAssemblyStatements selfType registerParamCount typeDetailsMap
      getAddress' = getAddress registerParamCount
      instOnly x = (x, [])
      -- scratch registers
      r1 = TwacR.R10
      r2 = TwacR.R11
      rCallee1 = TwacR.R12
      rCallee2 = TwacR.R13
      rCallee3 = TwacR.R14
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
          pure $ instOnly [SubtractImmediate64 (words * 8) TwacR.Rsp]
        TwacR.DeallocateStackSpace words ->
          pure $ instOnly [AddImmediate64 (words * 8) TwacR.Rsp]
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
                [ Load (attributeAddress src 0) r1,
                  Load (attributeAddress dst 0) TwacR.Rax,
                  Cdq,
                  Divide r1,
                  Store TwacR.Rax (attributeAddress dst 0)
                ]
          Twac.LessThan src1 src2 dst ->
            pure $
              instOnly
                [ Transfer src1 TwacR.Rdi,
                  Transfer src2 TwacR.Rsi,
                  Call $ Label "less_than",
                  Transfer TwacR.Rax dst
                ]
          Twac.LessThanOrEqualTo src1 src2 dst ->
            pure $
              instOnly
                [ Transfer src1 TwacR.Rdi,
                  Transfer src2 TwacR.Rsi,
                  Call $ Label "less_than_equal",
                  Transfer TwacR.Rax dst
                ]
          Twac.Equals src1 src2 dst ->
            pure $
              instOnly
                [ Transfer src1 TwacR.Rdi,
                  Transfer src2 TwacR.Rsi,
                  Call $ Label "equal",
                  Transfer TwacR.Rax dst
                ]
          Twac.IntConstant i dst -> do
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Int") dst
            pure $ instOnly $ construction ++ [LoadConst i r1, Store r1 (attributeAddress dst 0)]
          Twac.BoolConstant b dst -> do
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Bool") dst
            pure $ instOnly $ construction ++ [LoadConst (if b then 1 else 0) r1, Store r1 (attributeAddress dst 0)]
          Twac.StringConstant string dst -> do
            let s = sanitizeString string
            let stringLength = length string
            let typeDetails = typeDetailsMap Map.! InputIr.Type "String"
            stringConstantLabel <- getLabel

            pure
              ( [LoadLabel stringConstantLabel dst],
                [ StringConstant
                    { stringConstantLabel,
                      stringConstantObjectSize = 8 * typeSize typeDetails,
                      stringConstantTypeTag = typeTag typeDetails,
                      stringConstantVTable = Label "String..vtable",
                      stringConstantSize = stringLength,
                      stringConstantCapacity = stringLength,
                      stringConstant = s
                    }
                ]
              )
          Twac.Not dst ->
            pure $
              instOnly
                [ Load (attributeAddress dst 0) r1,
                  XorConst 1 r1,
                  Store r1 (attributeAddress dst 0)
                ]
          Twac.Negate dst ->
            pure $
              instOnly
                [ Load (attributeAddress dst 0) r1,
                  Negate r1,
                  Store r1 (attributeAddress dst 0)
                ]
          -- TODO: deal with SELF_TYPE, somehow (look at type tag of self?)
          -- Note: we are guaranteed to not need the self pointer in
          -- non-SELF_TYPE construction.
          Twac.New type' dst ->
            case type' of
              InputIr.Type "SELF_TYPE" ->
                pure $
                  instOnly
                    [ Push TwacR.Rdi,
                      Load (sizeAddress TwacR.R15) TwacR.Rdi,
                      LoadConst 1 TwacR.Rsi,
                      Call $ Label "calloc",
                      Transfer TwacR.Rax TwacR.Rdi,
                      Load (sizeAddress TwacR.R15) r1,
                      Store r1 (sizeAddress TwacR.Rdi),
                      Load (typeTagAddress TwacR.R15) r1,
                      Store r1 (typeTagAddress TwacR.Rdi),
                      Load (vTableAddress TwacR.R15) r1,
                      Store r1 (vTableAddress TwacR.Rdi),
                      Store TwacR.Rdi $ Address (Just 8) TwacR.Rsp Nothing Nothing,
                      CallAddress (vTableMethodAddress r1 0),
                      Load (Address (Just 8) TwacR.Rsp Nothing Nothing) TwacR.Rdi,
                      Transfer TwacR.Rdi dst,
                      Pop TwacR.Rdi
                    ]
              InputIr.Type typeName ->
                let TypeDetails tag size _ = typeDetailsMap Map.! type'
                    vtable = Label $ typeName ++ "..vtable"
                 in pure $
                      instOnly $
                        callocWords size
                          ++ [ Push TwacR.R15,
                               Transfer TwacR.Rax TwacR.R15,
                               StoreConst (size * 8) (sizeAddress TwacR.R15),
                               StoreConst tag (typeTagAddress TwacR.R15),
                               LoadLabel vtable r1,
                               Store r1 (vTableAddress TwacR.R15),
                               Transfer TwacR.R15 TwacR.Rdi,
                               SubtractImmediate64 8 TwacR.Rsp,
                               Call $ Label $ typeName ++ "..new",
                               AddImmediate64 8 TwacR.Rsp,
                               Transfer TwacR.R15 dst,
                               Pop TwacR.R15
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
          Twac.IsVoid dst -> do
            -- after new, the allocated object is in Rax
            (new, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Bool") dst
            nonVoidLabel <- getLabel
            endLabel <- getLabel
            set <- setBool dst JumpZero
            pure $ instOnly ([Transfer dst rCallee1] ++ new ++ [Test64 rCallee1 rCallee1] ++ set)
          Twac.Dispatch dispatchResult dispatchReceiver dispatchReceiverType dispatchType dispatchMethod dispatchArgs ->
            case dispatchType of
              -- Static dispatch; this is easy! We like this.
              Just (InputIr.Type t) -> pure $ instOnly [Call $ Label $ t ++ "." ++ dispatchMethod]
              Nothing ->
                let InputIr.Type dispatchReceiverTypeName = dispatchReceiverType
                    methodTag =
                      methodTags
                        ( typeDetailsMap
                            Map.! ( if dispatchReceiverTypeName == "SELF_TYPE"
                                      then selfType
                                      else dispatchReceiverType
                                  )
                        )
                        Map.! dispatchMethod
                 in pure $
                      instOnly
                        [ Load (vTableAddress dispatchReceiver) r1,
                          CallAddress (vTableMethodAddress r1 methodTag)
                        ]
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
                  Test64 r1 r1,
                  JumpNonZero label
                ]
          Twac.Assign src dst ->
            pure $ instOnly [Transfer src dst]
          Twac.Copy src dst -> do
            isNotVoidLabel <- getLabel
            endLabel <- getLabel
            pure $
              instOnly
                [ Comment $ "begin copy " ++ show src ++ " " ++ show dst,
                  -- if the src is void, we need to just copy the pointer
                  Test64 src src,
                  JumpNonZero isNotVoidLabel,
                  Transfer src dst,
                  Jump endLabel,
                  AssemblyLabel isNotVoidLabel,
                  Transfer src rCallee1,
                  -- allocate memory
                  Load (attributeAddress src $ -3) TwacR.Rdi,
                  Call $ Label "malloc",
                  Transfer TwacR.Rax rCallee2,
                  -- copy memory
                  Transfer rCallee2 TwacR.Rdi,
                  Transfer rCallee1 TwacR.Rsi,
                  Load (attributeAddress rCallee1 $ -3) TwacR.Rdx,
                  Call $ Label "memcpy",
                  -- save ptr to memory to dst
                  Transfer rCallee2 dst,
                  AssemblyLabel endLabel,
                  Comment $ "end copy " ++ show src ++ " " ++ show dst
                ]
          Twac.TwacCase conditionVariable jumpTable -> do
            jumpTableLabel <- getLabel
            -- TODO: check for case on void
            -- TODO: (PA4) we can definitely optimize this assembly
            let code =
                  [ LoadLabel jumpTableLabel r1,
                    Load (typeTagAddress conditionVariable) r2,
                    MultiplyConst 8 r2,
                    Add r2 r1,
                    JumpAddress (Address Nothing r1 Nothing Nothing)
                  ]
            pure (code, [JumpTable jumpTableLabel (Map.elems jumpTable)])
          Twac.Abort line reason ->
            let normalAbort messageName =
                  [ LoadLabel (Label messageName) TwacR.Rdi,
                    LoadConst line TwacR.Rsi,
                    Call $ Label "printf",
                    LoadConst 1 TwacR.Rdi,
                    Call $ Label "exit"
                  ]
             in pure
                  ( case reason of
                      Trac.DispatchOnVoid -> normalAbort "dispatch_on_void"
                      Trac.StaticDispatchOnVoid -> normalAbort "static_dispatch_on_void"
                      Trac.CaseOnVoid -> normalAbort "case_on_void"
                      Trac.CaseNoMatch typeName ->
                        [ LoadLabel (Label "case_no_match") TwacR.Rdi,
                          LoadConst line TwacR.Rsi,
                          Load (attributeAddress typeName 0) TwacR.Rdx,
                          Call $ Label "printf",
                          LoadConst 1 TwacR.Rdi,
                          Call $ Label "exit"
                        ]
                      Trac.DivisionByZero -> normalAbort "division_by_zero"
                      Trac.SubstringOutOfRange -> normalAbort "substring_out_of_range",
                    []
                  )
          -- TODO: replace commments with calls as we write the code
          Twac.TwacInternal internal ->
            let call function = pure $ instOnly [SubtractImmediate64 8 TwacR.Rsp, Call $ Label function, AddImmediate64 8 TwacR.Rsp, Return]
             in case internal of
                  InputIr.IOInInt -> call "in_int"
                  InputIr.IOInString -> call "in_string"
                  InputIr.IOOutInt -> call "out_int"
                  InputIr.IOOutString -> call "out_string"
                  InputIr.ObjectAbort -> call "abort"
                  InputIr.ObjectCopy -> call "copy"
                  InputIr.ObjectTypeName -> call "type_name"
                  InputIr.StringConcat -> call "concat"
                  InputIr.StringLength -> call "length"
                  InputIr.StringSubstr -> call "substr"

-- Inspired by the reference compiler output, though there really is just about
-- one way to do this. Using calloc instead of malloc means we can save the
-- multiply/bit shift for libc instead of doing it ourselves, and that makes us
-- happy.
calloc :: Int -> Int -> [AssemblyStatement]
calloc a b =
  [ LoadConst a TwacR.Rdi,
    LoadConst b TwacR.Rsi,
    Call $ Label "calloc"
  ]

callocWords = calloc 8

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
      LoadConst 1 TwacR.R11,
      Store TwacR.R11 (attributeAddress boolReg 0),
      AssemblyLabel falseLabel
    ]

-- asm strings can have escape codes
sanitizeString :: String -> String
sanitizeString = concatMap sanitizeChar

sanitizeChar :: Char -> [Char]
sanitizeChar '\\' = "\\\\"
sanitizeChar '\n' = "\\n"
sanitizeChar '\t' = "\\t"
sanitizeChar '"' = "\\\""
sanitizeChar c = [c]

-- haskell doesn't like circular dependencies 😭
main' :: TypeDetailsMap -> State Temporary ([AssemblyStatement], [AssemblyData])
main' typeDetailsMap = do
  let type' = InputIr.Type "Main"
  let label = Label "main"
  body <-
    combineAssembly
      <$> mapM
        (generateAssemblyStatements (InputIr.Type "") 1 typeDetailsMap)
        [ TwacRStatement $ Twac.TwacLabel label,
          TwacR.Push TwacR.Rbx,
          TwacR.Push TwacR.Rbp,
          TwacR.Push TwacR.R12,
          TwacR.Push TwacR.R13,
          TwacR.Push TwacR.R14,
          TwacR.Push TwacR.R15,
          TwacR.AllocateStackSpace 1,
          TwacRStatement $ Twac.New type' TwacR.Rdi,
          TwacRStatement $ Twac.Dispatch TwacR.R10 TwacR.Rdi type' Nothing "main" [TwacR.Rdi],
          TwacR.DeallocateStackSpace 1,
          TwacR.Pop TwacR.R15,
          TwacR.Pop TwacR.R14,
          TwacR.Pop TwacR.R13,
          TwacR.Pop TwacR.R12,
          TwacR.Pop TwacR.Rbp,
          TwacR.Pop TwacR.Rbx
        ]
  pure $
    combineAssembly
      [ ([Global label], []),
        body,
        -- TODO: should probably be a xor %rax %rax
        ([LoadConst 0 TwacR.Rdx, Return], [])
      ]

-- TODO: write error handling here
inInt typeDetailsMap =
  let formatLabel = Label "in_int_format"
      overflowLabel = Label "in_int_overflow"
      noOverflowLabel = Label "in_int_no_overflow"
      TypeDetails tag size _ = typeDetailsMap Map.! InputIr.Type "Int"
   in ( [AssemblyLabel $ Label "in_int"]
          -- since we calloc, the integer starts at zero
          ++ callocWords size
          ++ [ StoreConst (size * 8) (sizeAddress TwacR.Rax),
               StoreConst tag (typeTagAddress TwacR.Rax),
               Transfer TwacR.Rax TwacR.R14,
               -- keep the stack 16-byte aligned and give space for the char **
               -- and int * for getline
               SubtractImmediate64 24 TwacR.Rsp,
               Lea (Address (Just 16) TwacR.Rsp Nothing Nothing) TwacR.Rdi, -- char **
               StoreConst 0 (Address (Just 16) TwacR.Rsp Nothing Nothing),
               Lea (Address (Just 8) TwacR.Rsp Nothing Nothing) TwacR.Rsi, -- size_t * (size)
               StoreConst 0 (Address (Just 8) TwacR.Rsp Nothing Nothing),
               LoadLabel (Label "stdin") TwacR.Rdx, -- stdin
               Load (Address Nothing TwacR.Rdx Nothing Nothing) TwacR.Rdx,
               Call $ Label "getline",
               Load (Address (Just 16) TwacR.Rsp Nothing Nothing) TwacR.Rdi, -- char *
               LoadLabel formatLabel TwacR.Rsi, -- format
               Lea (attributeAddress TwacR.R14 0) TwacR.Rdx, -- long *
               Call $ Label "sscanf",
               AddImmediate64 24 TwacR.Rsp,
               -- TODO: Should probably be a cmov, eventually
               Load (attributeAddress TwacR.R14 0) TwacR.R13,
               CmpConst 2147483647 TwacR.R13,
               JumpGreaterThan overflowLabel,
               CmpConst (-2147483648) TwacR.R13,
               JumpLessThan overflowLabel,
               Jump noOverflowLabel,
               AssemblyLabel overflowLabel,
               StoreConst 0 (attributeAddress TwacR.R14 0),
               AssemblyLabel noOverflowLabel,
               Transfer TwacR.R14 TwacR.Rax,
               Return
             ],
        [RawStringConstant formatLabel "%ld"]
      )

outInt =
  let formatLabel = Label "out_int_format"
   in ( [ AssemblyLabel $ Label "out_int",
          Push TwacR.Rdi,
          LoadLabel formatLabel TwacR.Rdi,
          Load (attributeAddress TwacR.Rsi 0) TwacR.Rsi,
          -- keep the stack 16-byte aligned
          Call $ Label "printf",
          Pop TwacR.Rax,
          Return
        ],
        [RawStringConstant formatLabel "%d"]
      )

-- see out-string-state-machine.svg
outString =
  let loop = Label "out_string_loop"
      backslash = Label "out_string_backslash"
      outChar = Label "out_string_out_char"
      outNewline = Label "out_string_out_newline"
      outTab = Label "out_string_out_tab"
      outBackslashEnd = Label "out_string_out_bs_end"
      outBackslashRepeat = Label "out_string_out_bs_repeat"
      outBackslashChar = Label "out_string_out_bs_char"
      end = Label "out_string_end"

      clearRdi = LoadConst 0 TwacR.Rdi
      loadChar = LoadByte (Address Nothing TwacR.R12 Nothing Nothing) TwacR.Rdi
      incrementPtr = AddImmediate64 1 TwacR.R12
      decrementLength = SubtractImmediate 1 TwacR.R13
      putChar = Call $ Label "putchar"
   in ( [ AssemblyLabel $ Label "out_string",
          -- Store self so we can return it. This also aligns the stack.
          Push TwacR.Rdi,
          -- Clear Rdi.
          -- R12 is the string pointer, R13 is the remaining length. Note that
          -- we always increment the pointer in concert with decrementing the
          -- length.
          Load (attributeAddress TwacR.Rsi 0) TwacR.R12,
          Load (attributeAddress TwacR.Rsi 1) TwacR.R13,
          -- loop
          AssemblyLabel loop,
          Test64 TwacR.R13 TwacR.R13,
          JumpZero end,
          clearRdi,
          loadChar,
          incrementPtr,
          decrementLength,
          CmpConst32 (toInteger $ ord '\\') TwacR.Rdi,
          JumpZero backslash,
          -- out_char. we have intentional fallthrough here. The label is for
          -- debugging purposes, we never actually jump to it.
          AssemblyLabel outChar,
          putChar,
          Jump loop,
          -- backslash.
          AssemblyLabel backslash,
          Test64 TwacR.R13 TwacR.R13,
          JumpZero outBackslashEnd,
          clearRdi,
          loadChar,
          incrementPtr,
          decrementLength,
          CmpConst32 (toInteger $ ord 'n') TwacR.Rdi,
          JumpZero outNewline,
          CmpConst32 (toInteger $ ord 't') TwacR.Rdi,
          JumpZero outTab,
          CmpConst32 (toInteger $ ord '\\') TwacR.Rdi,
          JumpZero outBackslashRepeat,
          -- out_bs_char. we have intentional fallthrough here. The label is for
          -- debugging purposes, we never actually jump to it. we save the read char in %r14 while we are printing '\\'.
          AssemblyLabel outBackslashChar,
          Transfer TwacR.Rdi TwacR.R14,
          LoadConst (ord '\\') TwacR.Rdi,
          putChar,
          Transfer TwacR.R14 TwacR.Rdi,
          putChar,
          Jump loop,
          -- out_newline.
          AssemblyLabel outNewline,
          LoadConst (ord '\n') TwacR.Rdi,
          putChar,
          Jump loop,
          -- out_tab.
          AssemblyLabel outTab,
          LoadConst (ord '\t') TwacR.Rdi,
          putChar,
          Jump loop,
          -- out_bs_repeat.
          AssemblyLabel outBackslashRepeat,
          LoadConst (ord '\\') TwacR.Rdi,
          putChar,
          Jump backslash,
          -- out_bs. Same idea with the fallthrough as out_char.
          AssemblyLabel outBackslashEnd,
          LoadConst (ord '\\') TwacR.Rdi,
          putChar,
          -- end. we exit. there is intentional fall through here
          AssemblyLabel end,
          Pop TwacR.Rax,
          Return
        ],
        []
      )

inString :: TypeDetailsMap -> State Temporary ([AssemblyStatement], [AssemblyData])
inString typeDetailsMap =
  do
    loop <- getLabel
    error <- getLabel
    endLoop <- getLabel
    let registerParamCount = 2
        generateAssemblyStatements' = generateAssemblyStatements (InputIr.Type "") registerParamCount typeDetailsMap
    (newString, _) <- generateAssemblyStatements' $ TwacR.TwacRStatement $ Twac.New (InputIr.Type "String") TwacR.Rax
    pure
      ( [ AssemblyLabel $ Label "in_string",
          SubtractImmediate64 8 TwacR.Rsp
        ]
          ++ newString
          ++ [ AssemblyLabel loop,
               Push TwacR.Rdi,
               Push TwacR.Rax,
               Call $ Label "getchar",
               Transfer TwacR.Rax TwacR.Rsi,
               Pop TwacR.Rax,
               Pop TwacR.Rdi,
               CmpConst32 (toInteger $ ord '\n') TwacR.Rsi,
               JumpZero endLoop,
               CmpConst32 (-1) TwacR.Rsi, -- eof
               JumpZero endLoop,
               CmpConst32 0 TwacR.Rsi, -- null byte
               JumpZero error,
               Transfer TwacR.Rax TwacR.Rdi,
               Push TwacR.Rdi,
               Push TwacR.Rax,
               Call $ Label "push_string",
               Pop TwacR.Rax,
               Pop TwacR.Rdi,
               Jump loop,
               --
               AssemblyLabel error,
               StoreConst 0 $ attributeAddress TwacR.Rdi 0,
               StoreConst 0 $ attributeAddress TwacR.Rdi 1,
               StoreConst 0 $ attributeAddress TwacR.Rdi 2,
               --
               AssemblyLabel endLoop,
               AddImmediate64 8 TwacR.Rsp,
               Transfer TwacR.Rdi TwacR.Rax,
               Return
             ],
        []
      )

pushString :: State Temporary ([AssemblyStatement], [AssemblyData])
pushString = do
  dontExpand <- getLabel
  copyLoop <- getLabel
  endCopyLoop <- getLabel
  nonNull <- getLabel
  pure
    ( [ AssemblyLabel $ Label "push_string",
        Push TwacR.R10,
        Load (attributeAddress TwacR.Rdi 0) TwacR.R9, -- string pointer
        CmpConst32 0 TwacR.R9,
        JumpNonZero nonNull,
        -- string pointer is null, lets make an allocation for it
        Push TwacR.Rdi,
        Push TwacR.Rsi,
        LoadConst 1 TwacR.Rdi,
        LoadConst 8 TwacR.Rsi,
        Call $ Label "calloc",
        Pop TwacR.Rsi,
        Pop TwacR.Rdi,
        Store TwacR.Rax (attributeAddress TwacR.Rdi 0),
        StoreConst 8 (attributeAddress TwacR.Rdi 2), -- store capacity
        TransferConst 0 TwacR.Rcx,
        Jump dontExpand,
        AssemblyLabel nonNull,
        Load (attributeAddress TwacR.Rdi 2) TwacR.Rdx, -- capacity
        Load (attributeAddress TwacR.Rdi 1) TwacR.Rcx, -- length
        Cmp TwacR.Rcx TwacR.Rdx,
        JumpGreaterThan dontExpand,
        -- we need to expand the string
        AddImmediate 4 TwacR.Rdx, -- handle case of capacity 0
        MultiplyConst 2 TwacR.Rdx,
        -- new allocation
        Push TwacR.Rdx,
        Push TwacR.Rcx,
        Push TwacR.Rsi,
        Push TwacR.Rdi,
        LoadConst 1 TwacR.Rdi,
        Transfer TwacR.Rdx TwacR.Rsi,
        Call $ Label "calloc",
        Pop TwacR.Rdi,
        Pop TwacR.Rsi,
        Pop TwacR.Rcx,
        Pop TwacR.Rdx,
        --
        Load (attributeAddress TwacR.Rdi 0) TwacR.R10, -- old string pointer
        Store TwacR.Rax (attributeAddress TwacR.Rdi 0), -- set new string pointer
        Store TwacR.Rdx (attributeAddress TwacR.Rdi 2), -- store new capacity
        LoadConst 0 TwacR.R8,
        AssemblyLabel copyLoop,
        Cmp TwacR.R8 TwacR.Rcx,
        JumpLessThanEqual endCopyLoop,
        LoadByte (Address (Just 0) TwacR.R10 (Just TwacR.R8) (Just 1)) TwacR.R9,
        StoreByte TwacR.R9 $ Address (Just 0) TwacR.Rax (Just TwacR.R8) (Just 1),
        AddImmediate 1 TwacR.R8,
        Jump copyLoop,
        AssemblyLabel dontExpand,
        Load (attributeAddress TwacR.Rdi 0) TwacR.Rax,
        AssemblyLabel endCopyLoop,
        -- there is definetly space in the string for another character now
        StoreByte TwacR.Rsi $ Address (Just 0) TwacR.Rax (Just TwacR.Rcx) (Just 1),
        AddImmediate 1 TwacR.Rcx,
        Store TwacR.Rcx $ attributeAddress TwacR.Rdi 1,
        Pop TwacR.R10,
        Return
      ],
      []
    )

abort :: State Temporary ([AssemblyStatement], [AssemblyData])
abort = do
  abortString <- getLabel
  pure
    ( [ AssemblyLabel $ Label "abort",
        SubtractImmediate64 8 TwacR.Rsp,
        LoadLabel abortString TwacR.Rdi,
        Call $ Label "puts",
        LoadConst 0 TwacR.Rdi,
        Call $ Label "exit"
        -- dont need to cleanup because we wont return from exit
      ],
      [RawStringConstant abortString "abort"]
    )

errorMessages =
  pure
    ( [],
      [ RawStringConstant (Label "dispatch_on_void") "ERROR: %d: Exception: dispatch on void\\n",
        RawStringConstant (Label "static_dispatch_on_void") "ERROR: %d: Exception: static dispatch on void\\n",
        RawStringConstant (Label "case_on_void") "ERROR: %d: Exception: case on void\\n",
        -- using %s here is fine because we will only ever call printf with an assembly string constant that gets a null terminator automatically
        RawStringConstant (Label "case_no_match") "ERROR: %d: Exception: case without matching branch: %s(...)\\n",
        RawStringConstant (Label "division_by_zero") "ERROR: %d: Exception: division by zero\\n",
        RawStringConstant (Label "substring_out_of_range") "ERROR: %d: Exception: String.substr out of range\\n"
      ]
    )

concatString :: TypeDetailsMap -> State Temporary ([AssemblyStatement], [AssemblyData])
concatString typeDetailsMap = do
  let registerParamCount = 2
      generateAssemblyStatements' = generateAssemblyStatements (InputIr.Type "") registerParamCount typeDetailsMap
  (newString, _) <- generateAssemblyStatements' $ TwacR.TwacRStatement $ Twac.New (InputIr.Type "String") TwacR.R8
  pure
    ( [ AssemblyLabel $ Label "concat",
        SubtractImmediate64 8 TwacR.Rsp,
        Push TwacR.Rdi,
        Push TwacR.Rsi
      ]
        ++ newString
        ++ [ Pop TwacR.Rsi,
             Pop TwacR.Rdi,
             Push TwacR.Rdi,
             Push TwacR.Rsi,
             Push TwacR.R8,
             Load (attributeAddress TwacR.Rsi 1) TwacR.Rsi,
             Load (attributeAddress TwacR.Rdi 1) TwacR.Rdi,
             Add TwacR.Rdi TwacR.Rsi,
             Push TwacR.Rsi,
             LoadConst 1 TwacR.Rdi,
             Call $ Label "calloc",
             Pop TwacR.Rdx,
             Pop TwacR.R8,
             Pop TwacR.Rsi,
             Pop TwacR.Rdi,
             Store TwacR.Rax (attributeAddress TwacR.R8 0), -- string
             Store TwacR.Rdx (attributeAddress TwacR.R8 1), -- length
             Store TwacR.Rdx (attributeAddress TwacR.R8 2), -- capacity
             Push TwacR.Rsi,
             Load (attributeAddress TwacR.Rdi 0) TwacR.Rsi, -- memcpy src
             Load (attributeAddress TwacR.Rdi 1) TwacR.Rdx, -- memcpy length
             Transfer TwacR.Rax TwacR.Rdi, -- memcpy dst
             Push TwacR.R8,
             Push TwacR.Rax,
             Push TwacR.Rdx,
             Call $ Label "memcpy",
             Pop TwacR.R9,
             Pop TwacR.Rax,
             Pop TwacR.R8,
             Pop TwacR.Rsi,
             Load (attributeAddress TwacR.Rsi 1) TwacR.Rdx, -- memcpy length
             Load (attributeAddress TwacR.Rsi 0) TwacR.Rsi, -- memcpy src
             Transfer TwacR.Rax TwacR.Rdi, -- memcpy dst
             Add64 TwacR.R9 TwacR.Rdi, -- add length of first string
             SubtractImmediate64 8 TwacR.Rsp,
             Push TwacR.R8,
             Call $ Label "memcpy",
             Pop TwacR.Rax,
             AddImmediate64 16 TwacR.Rsp,
             Return
           ],
      []
    )

stringLength :: TypeDetailsMap -> State Temporary ([AssemblyStatement], [AssemblyData])
stringLength typeDetailsMap = do
  let registerParamCount = 1
      generateAssemblyStatements' = generateAssemblyStatements (InputIr.Type "") registerParamCount typeDetailsMap
  (newInt, _) <- generateAssemblyStatements' $ TwacR.TwacRStatement $ Twac.New (InputIr.Type "Int") TwacR.Rax
  pure
    ( [ AssemblyLabel $ Label "length",
        Push TwacR.Rdi
      ]
        ++ newInt
        ++ [ Pop TwacR.Rdi,
             Load (attributeAddress TwacR.Rdi 1) TwacR.R8,
             Store TwacR.R8 (attributeAddress TwacR.Rax 0),
             Return
           ],
      []
    )

stringSubstr :: TypeDetailsMap -> State Temporary ([AssemblyStatement], [AssemblyData])
stringSubstr typeDetailsMap = do
  let registerParamCount = 3
      generateAssemblyStatements' = generateAssemblyStatements (InputIr.Type "") registerParamCount typeDetailsMap
  (newString, _) <- generateAssemblyStatements' $ TwacR.TwacRStatement $ Twac.New (InputIr.Type "String") TwacR.Rax
  outOfRange <- getLabel
  pure
    ( [ AssemblyLabel $ Label "substr",
        Push TwacR.Rdi,
        Push TwacR.Rsi,
        Push TwacR.Rdx
      ]
        ++ newString
        ++ [ Pop TwacR.Rdx,
             Pop TwacR.Rsi,
             Pop TwacR.Rdi,
             Load (attributeAddress TwacR.Rdx 0) TwacR.Rdx, -- length
             Load (attributeAddress TwacR.Rsi 0) TwacR.Rsi, -- start idx
             Load (attributeAddress TwacR.Rdi 1) TwacR.R9, -- total length of original string
             CmpConst32 0 TwacR.Rsi,
             JumpLessThan outOfRange,
             CmpConst32 0 TwacR.Rdx,
             JumpLessThan outOfRange,
             Transfer TwacR.Rdx TwacR.R8,
             Add TwacR.Rsi TwacR.R8, -- end idx
             Cmp TwacR.R8 TwacR.R9,
             JumpLessThan outOfRange,
             --
             Load (attributeAddress TwacR.Rdi 0) TwacR.Rcx,
             Add TwacR.Rsi TwacR.Rcx,
             Store TwacR.Rcx (attributeAddress TwacR.Rax 0), -- string ptr
             Store TwacR.Rdx (attributeAddress TwacR.Rax 1), -- length
             Store TwacR.Rdx (attributeAddress TwacR.Rax 2), -- capacity
             Return,
             AssemblyLabel outOfRange,
             LoadLabel (Label "substring_out_of_range") TwacR.Rdi,
             LoadConst 0 TwacR.Rsi,
             SubtractImmediate64 8 TwacR.Rsp,
             Call $ Label "printf",
             Call $ Label "exit"
           ],
      []
    )

objectCopy =
  pure
    ( [ AssemblyLabel $ Label "copy",
        SubtractImmediate64 8 TwacR.Rsp,
        -- %r12 stores the original object
        Transfer TwacR.Rdi TwacR.R12,
        -- %r13 stores size
        Load (sizeAddress TwacR.Rdi) TwacR.R13,
        Transfer TwacR.R13 TwacR.Rdi,
        Call $ Label "malloc",
        -- %r14 stores the new object
        Transfer TwacR.Rax TwacR.R14,
        Transfer TwacR.Rax TwacR.Rdi,
        Transfer TwacR.R12 TwacR.Rsi,
        Transfer TwacR.R13 TwacR.Rdx,
        Call $ Label "memcpy",
        Transfer TwacR.R14 TwacR.Rax,
        AddImmediate64 8 TwacR.Rsp,
        Return
      ],
      []
    )

typeName :: TypeDetailsMap -> State Temporary ([AssemblyStatement], [AssemblyData])
typeName typeDetailsMap = do
  let typeDetailsList = Map.toAscList typeDetailsMap

  labels <- traverse (const getLabel) typeDetailsList

  let stringConstants =
        zipWith
          ( \label (InputIr.Type typeName, typeDetails) ->
              StringConstant
                { stringConstantLabel = label,
                  stringConstantObjectSize = 8 * typeSize typeDetails,
                  stringConstantTypeTag = typeTag typeDetails,
                  stringConstantVTable = Label "String..vtable",
                  stringConstantSize = length typeName,
                  stringConstantCapacity = length typeName,
                  stringConstant = typeName
                }
          )
          labels
          typeDetailsList

  let typeNameTable = Label "type_name_table"

  pure
    ( [ AssemblyLabel $ Label "type_name",
        LoadLabel typeNameTable TwacR.R10,
        Load (typeTagAddress TwacR.Rdi) TwacR.R11,
        Load (Address Nothing TwacR.R10 (Just TwacR.R11) (Just 8)) TwacR.Rax,
        Return
      ],
      TypeNameTable typeNameTable labels : stringConstants
    )

lessThanOrEqualTo :: TypeDetailsMap -> State Temporary ([AssemblyStatement], [AssemblyData])
lessThanOrEqualTo typeDetailsMap = do
  boolConstruction <-
    generateAssemblyStatements (InputIr.Type "") 1 typeDetailsMap $
      TwacRStatement $
        Twac.New (InputIr.Type "Bool") TwacR.R14

  let exitBlock label value =
        [ AssemblyLabel label,
          StoreConst value (attributeAddress TwacR.R14 0),
          Transfer TwacR.R14 TwacR.Rax,
          AddImmediate64 8 TwacR.Rsp,
          Return
        ]
  let trueLabel = Label "less_than_equal_true"
  let trueBlock = exitBlock trueLabel 1
  let falseLabel = Label "less_than_equal_false"
  let falseBlock = exitBlock falseLabel 0

  -- specific comparisons
  let boolIntCmpLabel = Label "less_than_equal_bool_int_cmp"
  let boolIntCmp =
        [ AssemblyLabel boolIntCmpLabel,
          -- do they both have the same type tag?
          Load (typeTagAddress TwacR.Rsi) TwacR.R11,
          Cmp TwacR.R10 TwacR.R11,
          JumpNonZero falseLabel,
          -- they do
          Load (attributeAddress TwacR.Rdi 0) TwacR.R10,
          Load (attributeAddress TwacR.Rsi 0) TwacR.R11,
          Cmp TwacR.R11 TwacR.R10,
          JumpLessThanEqual trueLabel,
          Jump falseLabel
        ]

  let strCmpLabel = Label "less_than_equal_str_cmp"
  let bLongerLabel = Label "less_than_equal_str_cmp_b_longer"
  let strCmp =
        [ AssemblyLabel strCmpLabel,
          -- do they both have the same type tag?
          Load (typeTagAddress TwacR.Rsi) TwacR.R11,
          Cmp TwacR.R10 TwacR.R11,
          JumpNonZero falseLabel,
          -- they do
          Load (attributeAddress TwacR.Rdi 1) TwacR.R10,
          Load (attributeAddress TwacR.Rsi 1) TwacR.R11,
          Cmp TwacR.R10 TwacR.R11,
          JumpGreaterThan bLongerLabel,
          -- we know len(a) <= len(b) here
          Load (attributeAddress TwacR.Rdi 0) TwacR.Rdi,
          Load (attributeAddress TwacR.Rsi 0) TwacR.Rsi,
          Transfer TwacR.R10 TwacR.Rdx,
          Call $ Label "memcmp",
          Test TwacR.Rax TwacR.Rax,
          JumpLessThanEqual trueLabel,
          Jump falseLabel,
          AssemblyLabel bLongerLabel,
          -- we know len(a) > len(b) here
          Load (attributeAddress TwacR.Rdi 0) TwacR.Rdi,
          Load (attributeAddress TwacR.Rsi 0) TwacR.Rsi,
          Transfer TwacR.R11 TwacR.Rdx,
          Call $ Label "memcmp",
          Test TwacR.Rax TwacR.Rax,
          JumpLessThan trueLabel,
          Jump falseLabel
        ]

  let objCmpLabel = Label "less_than_equal_obj_cmp"
  let objCmp =
        [ AssemblyLabel objCmpLabel,
          Cmp64 TwacR.Rdi TwacR.Rsi,
          JumpZero trueLabel,
          Jump falseLabel
        ]

  let getTypeTag typeName = toInteger $ typeTag $ typeDetailsMap Map.! InputIr.Type typeName

  -- here we jump to the appropriate specific comparison blocks when relevant. We consider a <= b.
  let decisionBlock =
        [ Pop TwacR.Rsi,
          Pop TwacR.Rdi,
          -- is a void? if so, go directly to object compare
          Test64 TwacR.Rsi TwacR.Rsi,
          JumpZero objCmpLabel,
          -- is b void? if so, since a is not void, this is false
          Test64 TwacR.Rdi TwacR.Rdi,
          JumpZero falseLabel,
          -- is a an int?
          Load (typeTagAddress TwacR.Rdi) TwacR.R10,
          CmpConst (getTypeTag "Int") TwacR.R10,
          JumpZero boolIntCmpLabel,
          -- is a a bool?
          CmpConst (getTypeTag "Bool") TwacR.R10,
          JumpZero boolIntCmpLabel,
          -- is a a string?
          CmpConst (getTypeTag "String") TwacR.R10,
          JumpZero strCmpLabel,
          -- it has to be *some* object
          Jump objCmpLabel
        ]

  pure $
    combineAssembly
      [ ( [ AssemblyLabel $ Label "less_than_equal",
            SubtractImmediate64 8 TwacR.Rsp,
            Push TwacR.Rdi,
            Push TwacR.Rsi
          ],
          []
        ),
        boolConstruction,
        ( decisionBlock ++ boolIntCmp ++ strCmp ++ objCmp ++ trueBlock ++ falseBlock,
          []
        )
      ]

equalTo :: TypeDetailsMap -> State Temporary ([AssemblyStatement], [AssemblyData])
equalTo typeDetailsMap = do
  boolConstruction <-
    generateAssemblyStatements (InputIr.Type "") 1 typeDetailsMap $
      TwacRStatement $
        Twac.New (InputIr.Type "Bool") TwacR.R14

  let exitBlock label value =
        [ AssemblyLabel label,
          StoreConst value (attributeAddress TwacR.R14 0),
          Transfer TwacR.R14 TwacR.Rax,
          AddImmediate64 8 TwacR.Rsp,
          Return
        ]
  let trueLabel = Label "equal_true"
  let trueBlock = exitBlock trueLabel 1
  let falseLabel = Label "equal_false"
  let falseBlock = exitBlock falseLabel 0

  -- specific comparisons
  let boolIntCmpLabel = Label "equal_bool_int_cmp"
  let boolIntCmp =
        [ AssemblyLabel boolIntCmpLabel,
          -- do they both have the same type tag?
          Load (typeTagAddress TwacR.Rsi) TwacR.R11,
          Cmp TwacR.R10 TwacR.R11,
          JumpNonZero falseLabel,
          -- they do
          Load (attributeAddress TwacR.Rdi 0) TwacR.R10,
          Load (attributeAddress TwacR.Rsi 0) TwacR.R11,
          Cmp TwacR.R11 TwacR.R10,
          JumpZero trueLabel,
          Jump falseLabel
        ]

  let strCmpLabel = Label "equal_str_cmp"
  let bLongerLabel = Label "equal_str_cmp_b_longer"
  let strCmp =
        [ AssemblyLabel strCmpLabel,
          -- do they both have the same type tag?
          Load (typeTagAddress TwacR.Rsi) TwacR.R11,
          Cmp TwacR.R10 TwacR.R11,
          JumpNonZero falseLabel,
          -- they do
          Load (attributeAddress TwacR.Rdi 1) TwacR.R10,
          Load (attributeAddress TwacR.Rsi 1) TwacR.R11,
          Cmp TwacR.R10 TwacR.R11,
          JumpNonZero falseLabel,
          -- we know len(a) == len(b) here
          Load (attributeAddress TwacR.Rdi 0) TwacR.Rdi,
          Load (attributeAddress TwacR.Rsi 0) TwacR.Rsi,
          Transfer TwacR.R10 TwacR.Rdx,
          Call $ Label "memcmp",
          Test TwacR.Rax TwacR.Rax,
          JumpZero trueLabel,
          Jump falseLabel,
          AssemblyLabel bLongerLabel
        ]

  let objCmpLabel = Label "equal_obj_cmp"
  let objCmp =
        [ AssemblyLabel objCmpLabel,
          Cmp64 TwacR.Rdi TwacR.Rsi,
          JumpZero trueLabel,
          Jump falseLabel
        ]

  let getTypeTag typeName = toInteger $ typeTag $ typeDetailsMap Map.! InputIr.Type typeName

  -- here we jump to the appropriate specific comparison blocks when relevant. We consider a <= b.
  let decisionBlock =
        [ Pop TwacR.Rsi,
          Pop TwacR.Rdi,
          -- is a void? if so, go directly to object compare
          Test64 TwacR.Rsi TwacR.Rsi,
          JumpZero objCmpLabel,
          -- is b void? if so, since a is not void, this is false
          Test64 TwacR.Rdi TwacR.Rdi,
          JumpZero falseLabel,
          -- is a an int?
          Load (typeTagAddress TwacR.Rdi) TwacR.R10,
          CmpConst (getTypeTag "Int") TwacR.R10,
          JumpZero boolIntCmpLabel,
          -- is a a bool?
          CmpConst (getTypeTag "Bool") TwacR.R10,
          JumpZero boolIntCmpLabel,
          -- is a a string?
          CmpConst (getTypeTag "String") TwacR.R10,
          JumpZero strCmpLabel,
          -- it has to be *some* object
          Jump objCmpLabel
        ]

  pure $
    combineAssembly
      [ ( [ AssemblyLabel $ Label "equal",
            SubtractImmediate64 8 TwacR.Rsp,
            Push TwacR.Rdi,
            Push TwacR.Rsi
          ],
          []
        ),
        boolConstruction,
        ( decisionBlock ++ boolIntCmp ++ strCmp ++ objCmp ++ trueBlock ++ falseBlock,
          []
        )
      ]

lessThan :: TypeDetailsMap -> State Temporary ([AssemblyStatement], [AssemblyData])
lessThan typeDetailsMap = do
  boolConstruction <-
    generateAssemblyStatements (InputIr.Type "") 1 typeDetailsMap $
      TwacRStatement $
        Twac.New (InputIr.Type "Bool") TwacR.R14

  let exitBlock label value =
        [ AssemblyLabel label,
          StoreConst value (attributeAddress TwacR.R14 0),
          Transfer TwacR.R14 TwacR.Rax,
          AddImmediate64 8 TwacR.Rsp,
          Return
        ]
  let trueLabel = Label "less_than_true"
  let trueBlock = exitBlock trueLabel 1
  let falseLabel = Label "less_than_false"
  let falseBlock = exitBlock falseLabel 0

  -- specific comparisons
  let boolIntCmpLabel = Label "less_than_bool_int_cmp"
  let boolIntCmp =
        [ AssemblyLabel boolIntCmpLabel,
          -- do they both have the same type tag?
          Load (typeTagAddress TwacR.Rsi) TwacR.R11,
          Cmp TwacR.R10 TwacR.R11,
          JumpNonZero falseLabel,
          -- they do
          Load (attributeAddress TwacR.Rdi 0) TwacR.R10,
          Load (attributeAddress TwacR.Rsi 0) TwacR.R11,
          Cmp TwacR.R11 TwacR.R10,
          JumpLessThan trueLabel,
          Jump falseLabel
        ]

  let strCmpLabel = Label "less_than_str_cmp"
  let bLongerLabel = Label "less_than_str_cmp_b_longer"
  let strCmp =
        [ AssemblyLabel strCmpLabel,
          -- do they both have the same type tag?
          Load (typeTagAddress TwacR.Rsi) TwacR.R11,
          Cmp TwacR.R10 TwacR.R11,
          JumpNonZero falseLabel,
          -- they do
          Load (attributeAddress TwacR.Rdi 1) TwacR.R10,
          Load (attributeAddress TwacR.Rsi 1) TwacR.R11,
          Cmp TwacR.R10 TwacR.R11,
          JumpGreaterThan bLongerLabel,
          -- we know len(a) <= len(b) here
          Load (attributeAddress TwacR.Rdi 0) TwacR.Rdi,
          Load (attributeAddress TwacR.Rsi 0) TwacR.Rsi,
          Transfer TwacR.R10 TwacR.Rdx,
          Call $ Label "memcmp",
          Test TwacR.Rax TwacR.Rax,
          JumpLessThan trueLabel,
          Jump falseLabel,
          AssemblyLabel bLongerLabel,
          -- we know len(a) > len(b) here
          Load (attributeAddress TwacR.Rdi 0) TwacR.Rdi,
          Load (attributeAddress TwacR.Rsi 0) TwacR.Rsi,
          Transfer TwacR.R11 TwacR.Rdx,
          Call $ Label "memcmp",
          Test TwacR.Rax TwacR.Rax,
          JumpLessThan trueLabel,
          Jump falseLabel
        ]

  let getTypeTag typeName = toInteger $ typeTag $ typeDetailsMap Map.! InputIr.Type typeName

  -- here we jump to the appropriate specific comparison blocks when relevant. We consider a <= b.
  let decisionBlock =
        [ Pop TwacR.Rsi,
          Pop TwacR.Rdi,
          -- is a void? if so, this is false
          Test64 TwacR.Rsi TwacR.Rsi,
          JumpZero falseLabel,
          -- is b void? if so, this is false
          Test64 TwacR.Rdi TwacR.Rdi,
          JumpZero falseLabel,
          -- is a an int?
          Load (typeTagAddress TwacR.Rdi) TwacR.R10,
          CmpConst (getTypeTag "Int") TwacR.R10,
          JumpZero boolIntCmpLabel,
          -- is a a bool?
          CmpConst (getTypeTag "Bool") TwacR.R10,
          JumpZero boolIntCmpLabel,
          -- is a a string?
          CmpConst (getTypeTag "String") TwacR.R10,
          JumpZero strCmpLabel,
          -- it has to be *some* object; objects cannot be less than each other
          Jump falseLabel
        ]

  pure $
    combineAssembly
      [ ( [ AssemblyLabel $ Label "less_than",
            SubtractImmediate64 8 TwacR.Rsp,
            Push TwacR.Rdi,
            Push TwacR.Rsi
          ],
          []
        ),
        boolConstruction,
        ( decisionBlock ++ boolIntCmp ++ strCmp ++ trueBlock ++ falseBlock,
          []
        )
      ]
