{-# LANGUAGE NamedFieldPuns #-}

module Assembly where

import Control.Monad.State
import Data.Char (ord)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybe)
import Distribution.Compat.CharParsing (CharParsing (string))
import qualified InputIr
import Trac (Label (..), Temporary (Temporary), TypeDetails (TypeDetails, typeSize), TypeDetailsMap, Variable (AttributeV, ParameterV, TemporaryV), getLabel, getVariable)
import qualified Twac
import TwacR (TwacRStatement (TwacRStatement), showByte)
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
  | CmpConst Integer TwacR.Register
  | XorConst Integer TwacR.Register
  | Store TwacR.Register Address
  | StoreConst Int Address
  | Lea Address TwacR.Register
  | Load Address TwacR.Register
  | LoadByte Address TwacR.Register
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
        binary op src dst = indent $ op ++ " " ++ show src ++ ", " ++ show dst
        binaryConst op src dst = indent $ op ++ " $" ++ show src ++ ", " ++ show dst
     in case instruction of
          Add src dst -> binary "addq" src dst
          AddImmediate src dst -> binaryConst "addq" src dst
          Subtract src dst -> binary "subq" src dst
          SubtractImmediate src dst -> binaryConst "subq" src dst
          Multiply src dst -> binary "imulq" src dst
          Divide src -> unary "idivq" src
          Cqto -> indent "cqto"
          Cmp src dst -> binary "cmpq" src dst
          Test src dst -> binary "testq" src dst
          TestConst src dst -> binary "testq" src dst
          CmpConst src dst -> binaryConst "cmpq" src dst
          XorConst src dst -> binaryConst "xorq" src dst
          Store src dst -> binary "movq" src dst
          StoreConst src dst -> binaryConst "movq" src dst
          Lea src dst -> binary "leaq" src dst
          Load src dst -> binary "movq" src dst
          LoadByte src dst -> indent $ "mov " ++ show src ++ ", " ++ showByte dst
          LoadConst src dst -> binaryConst "movq" src dst
          Transfer src dst -> binary "movq" src dst
          Push src -> unary "pushq" src
          Pop dst -> unary "popq" dst
          Not dst -> unary "notq" dst
          Negate dst -> unary "negq" dst
          AssemblyLabel (Label label) -> label ++ ":"
          Call label -> unary "call" label
          DynamicCall label -> unary "call" label
          Return -> indent "ret"
          Jump label -> unary "jmp" label
          JumpZero label -> unary "jz" label
          JumpNonZero label -> unary "jnz" label
          JumpLessThan label -> unary "jl" label
          JumpLessThanEqual label -> unary "jle" label
          LoadLabel label dst -> binary "leaq" label dst
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

-- TODO: handle the other types of data (not needed for PA3c3)
instance Show AssemblyData where
  show data' = case data' of
    StringConstant label text -> show label ++ ": .string \"" ++ sanitizeString text ++ "\""

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

-- TODO: don't just shove main at the beginning.
instance Show AssemblyIr where
  show (AssemblyIr code data') = unlines (map show data') ++ "\n\n.globl main\n" ++ unlines (map show code)

-- TODO: actually generate constructors...

main' = ([AssemblyLabel $ Label "main", Jump $ Label "Main.main"], [])

-- TODO: write error handling here
inInt typeDetailsMap =
  let formatLabel = Label "in_int_format"
      TypeDetails tag size = typeDetailsMap Map.! InputIr.Type "Int"
   in ( [AssemblyLabel $ Label "in_int"]
          ++ callocWords size
          ++ [ StoreConst (size * 8) (sizeAddress TwacR.Rax),
               StoreConst tag (typeTagAddress TwacR.Rax),
               Transfer TwacR.Rax TwacR.R14,
               LoadLabel formatLabel TwacR.Rdi,
               Lea (attributeAddress TwacR.R14 0) TwacR.Rsi,
               -- keep the stack 16-byte aligned
               SubtractImmediate 8 TwacR.Rsp,
               Call $ Label "scanf",
               AddImmediate 8 TwacR.Rsp,
               Transfer TwacR.R14 TwacR.Rax,
               Return
             ],
        [StringConstant formatLabel "%d"]
      )

outInt =
  let formatLabel = Label "out_int_format"
   in ( [ AssemblyLabel $ Label "out_int",
          LoadLabel formatLabel TwacR.Rdi,
          Load (attributeAddress TwacR.Rsi 0) TwacR.Rsi,
          -- keep the stack 16-byte aligned
          SubtractImmediate 8 TwacR.Rsp,
          Call $ Label "printf",
          AddImmediate 8 TwacR.Rsp,
          Return
        ],
        [StringConstant formatLabel "%d"]
      )

outString =
  let loop = Label "out_string_loop"
      nonBackslash = Label "out_string_non_backslash"
      notEscapeSequence = Label "out_string_not_escape"
      notPrevBackslash = Label "out_string_not_prev_backslash"
      nonNewline = Label "out_string_newline"
      afterCall = Label "out_string_after_call"
      end = Label "out_string_end"
   in ( [ AssemblyLabel $ Label "out_string",
          Load (attributeAddress TwacR.Rsi 0) TwacR.R8,
          Load (attributeAddress TwacR.Rsi 1) TwacR.Rcx,
          LoadConst 0 TwacR.Rsi, -- keep track of backslashes
          LoadConst 0 TwacR.Rdx, -- loop counter
          AssemblyLabel loop,
          LoadConst 0 TwacR.Rdi,
          LoadByte (Address Nothing TwacR.R8 (Just TwacR.Rdx) (Just 1)) TwacR.Rdi,
          CmpConst 0 TwacR.Rsi,
          JumpZero notPrevBackslash,
          -- previous character was a backslash
          LoadConst 0 TwacR.Rsi, -- reset flag
          CmpConst (toInteger $ ord 'n') TwacR.Rdi,
          JumpNonZero nonNewline,
          -- prev was a backslash and current is a 'n'
          LoadConst (ord '\n') TwacR.Rdi,
          Jump nonBackslash,
          AssemblyLabel nonNewline,
          -- prev character was a backslash but cur isnt 'n'
          CmpConst (toInteger $ ord 't') TwacR.Rdi,
          JumpNonZero notEscapeSequence,
          -- prev character was backslash and cur is 't'
          LoadConst (ord '\t') TwacR.Rdi,
          Jump nonBackslash,
          AssemblyLabel notEscapeSequence,
          -- prev was backslash, current char isnt an escape sequence
          -- need to output an extra backslash
          Push TwacR.Rcx,
          Push TwacR.Rdi,
          Push TwacR.Rsi,
          Push TwacR.Rdx,
          Push TwacR.R8,
          LoadConst (ord '\\') TwacR.Rdi,
          Call $ Label "putchar",
          Pop TwacR.R8,
          Pop TwacR.Rdx,
          Pop TwacR.Rsi,
          Pop TwacR.Rdi,
          Pop TwacR.Rcx,
          AssemblyLabel notPrevBackslash,
          CmpConst (toInteger $ ord '\\') TwacR.Rdi,
          JumpNonZero nonBackslash,
          -- cur character is backslash
          LoadConst 1 TwacR.Rsi,
          Jump afterCall,
          AssemblyLabel nonBackslash,
          Push TwacR.Rcx,
          Push TwacR.Rdi,
          Push TwacR.Rsi,
          Push TwacR.Rdx,
          Push TwacR.R8,
          Call $ Label "putchar",
          Pop TwacR.R8,
          Pop TwacR.Rdx,
          Pop TwacR.Rsi,
          Pop TwacR.Rdi,
          Pop TwacR.Rcx,
          AssemblyLabel afterCall,
          AddImmediate 1 TwacR.Rdx,
          Cmp TwacR.Rcx TwacR.Rdx,
          JumpLessThan loop,
          CmpConst 0 TwacR.Rsi,
          JumpZero end,
          -- ended the loop with a backslash buffered, need to output it
          Push TwacR.R8, -- just need to push something for parity here
          LoadConst (ord '\\') TwacR.Rdi,
          Call $ Label "putchar",
          Pop TwacR.R8,
          AssemblyLabel end,
          Return
        ],
        []
      )

generateAssembly :: Temporary -> TwacR.TwacRIr -> AssemblyIr
generateAssembly temporaryState TwacR.TwacRIr {TwacR.implementationMap, TwacR.constructorMap, TwacR.typeDetailsMap} =
  uncurry AssemblyIr $
    combineAssembly
      [ main',
        inInt typeDetailsMap,
        outInt,
        outString,
        evalState
          ( do
              let methodList = map snd $ Map.toList implementationMap
              x <- traverse (traverse $ generateAssemblyMethod typeDetailsMap) methodList
              let (code, data') = traverse combineAssembly x
              pure (code, concat data')
          )
          temporaryState
      ]

generateAssemblyMethod :: TypeDetailsMap -> TwacR.TwacRMethod -> State Temporary ([AssemblyStatement], [AssemblyData])
generateAssemblyMethod typeDetailsMap method = do
  lines <-
    traverse
      (generateAssemblyStatements (TwacR.registerParamCount method) typeDetailsMap . item)
      (TwacR.body method)
  -- TODO: this is a hack for PA3c3.
  pure $
    if TwacR.methodName method == "main"
      then combineAssembly lines
      else ([], [])

combineAssembly :: [([AssemblyStatement], [AssemblyData])] -> ([AssemblyStatement], [AssemblyData])
combineAssembly asm =
  let statements = concatMap fst asm
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
vtableAddress :: TwacR.Register -> Address
vtableAddress r = Address (Just 16) r Nothing Nothing

-- Gives the address of the nth attribute pointed to by the given register
attributeAddress :: TwacR.Register -> Int -> Address
attributeAddress reg n = Address (Just $ (n + 3) * 8) reg Nothing Nothing

generateAssemblyStatements :: Int -> TypeDetailsMap -> TwacR.TwacRStatement -> State Temporary ([AssemblyStatement], [AssemblyData])
generateAssemblyStatements registerParamCount typeDetailsMap twacRStatement =
  let generateAssemblyStatements' = generateAssemblyStatements registerParamCount typeDetailsMap
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
          pure $ instOnly [SubtractImmediate (words * 8) TwacR.Rsp]
        TwacR.DeallocateStackSpace words ->
          pure $ instOnly [AddImmediate (words * 8) TwacR.Rsp]
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
                  Cqto,
                  Divide r1,
                  Store TwacR.Rax (attributeAddress dst 0)
                ]
          -- TODO: make comparisons handle things that are not booleans or ints
          Twac.LessThan src dst -> do
            let load =
                  [ Load (attributeAddress src 0) rCallee1,
                    Load (attributeAddress dst 0) rCallee2,
                    Transfer dst rCallee3
                  ]
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Bool") rCallee3
            setting <- setBool rCallee3 JumpLessThan
            pure $ instOnly $ load ++ construction ++ [Cmp rCallee1 rCallee2] ++ setting ++ [Transfer rCallee3 dst]
          Twac.LessThanOrEqualTo src dst -> do
            let load =
                  [ Load (attributeAddress src 0) rCallee1,
                    Load (attributeAddress dst 0) rCallee2,
                    Transfer dst rCallee3
                  ]
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Bool") rCallee3
            setting <- setBool rCallee3 JumpLessThanEqual
            pure $ instOnly $ load ++ construction ++ [Cmp rCallee1 rCallee2] ++ setting ++ [Transfer rCallee3 dst]
          Twac.Equals src dst -> do
            let load =
                  [ Load (attributeAddress src 0) rCallee1,
                    Load (attributeAddress dst 0) rCallee2,
                    Transfer dst rCallee3
                  ]
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Bool") rCallee3
            setting <- setBool rCallee3 JumpZero
            pure $ instOnly $ load ++ construction ++ [Cmp rCallee1 rCallee2] ++ setting ++ [Transfer rCallee3 dst]
          Twac.IntConstant i dst -> do
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Int") dst
            pure $ instOnly $ construction ++ [LoadConst i r1, Store r1 (attributeAddress dst 0)]
          Twac.BoolConstant b dst -> do
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "Bool") dst
            pure $ instOnly $ construction ++ [LoadConst (if b then 1 else 0) r1, Store r1 (attributeAddress dst 0)]
          Twac.StringConstant s dst -> do
            (construction, _) <- generateAssemblyStatements' $ TwacRStatement $ Twac.New (InputIr.Type "String") dst
            stringLabel <- getLabel
            pure
              ( construction
                  ++ [ LoadLabel stringLabel r1,
                       Store r1 (attributeAddress dst 0),
                       StoreConst (length s) (attributeAddress dst 1)
                     ],
                [StringConstant stringLabel s]
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
          -- TODO: set vtable pointer
          -- TODO: deal with SELF_TYPE, somehow (look at type tag of self?)
          Twac.New type' dst ->
            let TypeDetails tag size = typeDetailsMap Map.! type'
             in pure $
                  instOnly $
                    callocWords size
                      ++ [ Transfer TwacR.Rax dst,
                           -- accessing negative attributes is a cursed, but
                           -- correct way of doing this.
                           StoreConst (size * 8) (sizeAddress dst),
                           StoreConst tag (typeTagAddress dst)
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
            set <- setBool TwacR.Rax JumpNonZero
            pure $ instOnly (new ++ set)
          -- TODO: make this not *incredibly* janky, lol. This *only* handles in_int and out_int.
          Twac.Dispatch dispatchResult dispatchReceiver dispatchReceiverType dispatchType dispatchMethod dispatchArgs ->
            case dispatchMethod of
              "in_int" -> pure $ instOnly [Call $ Label "in_int"]
              "out_int" -> pure $ instOnly [Call $ Label "out_int"]
              "out_string" -> pure $ instOnly [Call $ Label "out_string"]
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
          Twac.Copy src dst ->
            pure $
              instOnly
                [ Comment $ "begin copy " ++ show src ++ " " ++ show dst,
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
                  Comment $ "end copy " ++ show src ++ " " ++ show dst
                ]
          Twac.TwacCase condition jmpTable -> undefined
          Twac.Abort line string -> undefined

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
sanitizeString = concatMap (\c -> if c == '\\' then "\\\\" else [c])
