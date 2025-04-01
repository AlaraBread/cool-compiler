{-# LANGUAGE NamedFieldPuns #-}

module TwacR where

import Control.Exception (assert)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import InputIr (Formal, ImplementationMapEntry, Type (Type))
import Trac (Label (Label), TypeDetailsMap, Variable (ParameterV))
import Twac
import Util

data TwacRIr = TwacRIr
  { implementationMap :: Map.Map Type [ImplementationMapEntry TwacRMethod],
    typeDetailsMap :: TypeDetailsMap
  }

data TwacRMethod = TwacRMethod {methodName :: String, body :: TwacR, registerParamCount :: Int, stackParamCount :: Int}

-- Twac with Register allocation, and consequently load/store operations.
type TwacR = [Lined TwacRStatement]

data TwacRStatement
  = TwacRStatement (TwacStatement Register)
  | Load Variable Register
  | Store Register Variable
  | Push Register
  | Pop Register
  | -- This is in words (8 bytes)
    AllocateStackSpace Int
  | DeallocateStackSpace Int

instance Show TwacRStatement where
  show twacRStatement = case twacRStatement of
    TwacRStatement twacStatement -> show twacStatement
    Load src dst -> show dst ++ " <- " ++ show src
    Store src dst -> show dst ++ " <- " ++ show src
    Push src -> "push " ++ show src
    Pop dst -> "pop " ++ show dst
    AllocateStackSpace n -> "stack_allocate " ++ show n
    DeallocateStackSpace n -> "stack_deallocate " ++ show n

data Register
  = Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | Rsp
  | Rbp
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Eq, Ord)

instance Show Register where
  show reg = case reg of
    Rax -> "%rax"
    Rbx -> "%rbx"
    Rcx -> "%rcx"
    Rdx -> "%rdx"
    Rsi -> "%rsi"
    Rdi -> "%rdi"
    Rsp -> "%rsp"
    Rbp -> "%rbp"
    R8 -> "%r8"
    R9 -> "%r9"
    R10 -> "%r10"
    R11 -> "%r11"
    R12 -> "%r12"
    R13 -> "%r13"
    R14 -> "%r14"
    R15 -> "%r15"

showByte reg = case reg of
  Rax -> "%al"
  Rbx -> "%bl"
  Rcx -> "%cl"
  Rdx -> "%dl"
  Rsi -> "%sil"
  Rdi -> "%dil"
  Rsp -> "%spl"
  Rbp -> "%bpl"
  R8 -> "%r8b"
  R9 -> "%r9b"
  R10 -> "%r10b"
  R11 -> "%r11b"
  R12 -> "%r12b"
  R13 -> "%r13b"
  R14 -> "%r14b"
  R15 -> "%r15b"

show32 reg = case reg of
  Rax -> "%eax"
  Rbx -> "%ebx"
  Rcx -> "%ecx"
  Rdx -> "%edx"
  Rsi -> "%esi"
  Rdi -> "%edi"
  Rsp -> "%esp"
  Rbp -> "%ebp"
  R8 -> "%r8d"
  R9 -> "%r9d"
  R10 -> "%r10d"
  R11 -> "%r11d"
  R12 -> "%r12d"
  R13 -> "%r13d"
  R14 -> "%r14d"
  R15 -> "%r15d"

allRegisters = Set.fromList [Rax, Rbx, Rcx, Rdx, Rsi, Rsp, Rbp, R8, R9, R10, R11, R12, R13, R14, R15]

paramRegisters = [Rdi, Rsi, Rdx, Rcx, R8, R9]

calleeSavedRegisters = [Rbp, Rbx, R12, R13, R14, R15]

-- This is a really inefficient way of doing this, at least for Rax and Rdx.
-- Anyways...
reservedRegisters =
  Set.fromList
    [ Rsp, -- stack pointer
      Rbp, -- frame pointer
      Rax, -- needed for division
      Rdx, -- needed for division
      R15, -- &self. Intentionally callee-saved and otherwise uninteresting.
      R10, -- scratch register for codegen
      R11, -- scratch register for codegen
      R12, -- callee-saved register for codegen
      R13, -- callee-saved register for codegen
      R14 -- callee-saved register for codegen
    ]

freeRegisters = Set.difference allRegisters reservedRegisters

-- This maximally inserts loads and stores. We will worry about this later :).
generateTwacRStatements :: TwacR -> Set.Set Register -> TwacIStatement -> [TwacRStatement]
generateTwacRStatements epilogue freeRegisters twac =
  let -- The invariant on all of these is that any registers they consume should
      -- be free-to-use after they run.

      unaryOperation :: (Register -> TwacStatement Register) -> Variable -> [TwacRStatement]
      unaryOperation op dst =
        let (dstR, freeRegisters') = Set.deleteFindMin freeRegisters
         in [ Load dst dstR,
              TwacRStatement $ op dstR,
              Store dstR dst
            ]

      binaryOperation :: (Register -> Register -> TwacStatement Register) -> Variable -> Variable -> [TwacRStatement]
      binaryOperation op src dst =
        let (srcR, freeRegisters') = Set.deleteFindMin freeRegisters
            (dstR, freeRegisters'') = Set.deleteFindMin freeRegisters'
         in [ Load src srcR,
              Load dst dstR,
              TwacRStatement $ op srcR dstR,
              Store dstR dst
            ]

      -- Same as unary, but we skip the load because we are guaranteed to
      -- immediately overwrite it anyways.
      immediate :: (Register -> TwacStatement Register) -> Variable -> [TwacRStatement]
      immediate op dst =
        let (dstR, freeRegisters') = Set.deleteFindMin freeRegisters
         in [ TwacRStatement $ op dstR,
              Store dstR dst
            ]
   in case twac of
        Add src dst -> binaryOperation Add src dst
        Subtract src dst -> binaryOperation Subtract src dst
        Multiply src dst -> binaryOperation Multiply src dst
        Divide src dst -> binaryOperation Divide src dst
        LessThan src dst -> binaryOperation LessThan src dst
        LessThanOrEqualTo src dst -> binaryOperation LessThanOrEqualTo src dst
        Equals src dst -> binaryOperation Equals src dst
        IntConstant i v -> immediate (IntConstant i) v
        BoolConstant i v -> immediate (BoolConstant i) v
        StringConstant i v -> immediate (StringConstant i) v
        Not dst -> unaryOperation Not dst
        Negate dst -> unaryOperation Negate dst
        -- These can use the immediate load/store sequence because they discard
        -- the existing value of dst, just like immediates do.
        New t dst -> immediate (New t) dst
        Default t dst -> immediate (Default t) dst
        IsVoid dst -> unaryOperation IsVoid dst
        Dispatch
          { dispatchResult,
            dispatchReceiver,
            dispatchReceiverType,
            dispatchType,
            dispatchMethod,
            dispatchArgs
          } ->
            let dispatchArgs' = dispatchReceiver : dispatchArgs
                argsR = zipWith Load dispatchArgs' paramRegisters
                memoryArgs = drop (length paramRegisters) dispatchArgs'
                -- stack needs to be 16 byte aligned
                memoryArgs' = if even $ length memoryArgs then memoryArgs else memoryArgs ++ [head memoryArgs]
             in concatMap
                  (\v -> [Load v Rdi, Push Rdi])
                  (reverse memoryArgs')
                  ++ argsR
                  ++ [ TwacRStatement $ Dispatch Rax Rdi dispatchReceiverType dispatchType dispatchMethod (take (length dispatchArgs') paramRegisters),
                       Store Rax dispatchResult
                     ]
        Jump label -> [TwacRStatement $ Jump label]
        TwacLabel label -> [TwacRStatement $ TwacLabel label]
        -- Even though we normally do not use Rax because division expects it to
        -- be available, Return is guaranteed to be the last thing in a
        -- function.
        Return ret ->
          Load ret Rax
            : map item epilogue
            ++ [TwacRStatement $ Return Rax]
        Comment comment -> [TwacRStatement $ Comment comment]
        ConditionalJump v label ->
          let (vR, freeRegisters') = Set.deleteFindMin freeRegisters
           in [ Load v vR,
                TwacRStatement $ ConditionalJump vR label
              ]
        Assign src dst ->
          let (vR, freeRegisters') = Set.deleteFindMin freeRegisters
           in [ Load src vR,
                Store vR dst
              ]
        Copy src dst -> binaryOperation Copy src dst
        TwacCase v jmpTable ->
          let (vR, freeRegisters') = Set.deleteFindMin freeRegisters
           in [ Load v vR,
                TwacRStatement $ TwacCase vR jmpTable
              ]
        Abort line message -> [TwacRStatement $ Abort line message]
        TwacInternal internal -> [TwacRStatement $ TwacInternal internal]

generateTwacR :: TwacR -> TwacI -> TwacR
generateTwacR epilogue = concatMap (unsequence . fmap (generateTwacRStatements epilogue freeRegisters))

generateTwacRMethod :: Type -> TwacMethod Variable -> TwacRMethod
generateTwacRMethod (Type typeName) (TwacMethod name body formals temporaryCount) =
  let registerParamCount = (min 6 $ 1 + length formals)
      memoryParamCount = (max 0 $ 1 - 6 + length formals)
      -- This is always 16n+8 bytes, to keep the stack 16-byte aligned. Note the
      -- return address unaligns the stack by 8 bytes, so this cancels out.
      temporarySpace = temporaryCount + ((temporaryCount + registerParamCount + length calleeSavedRegisters) `rem` 2) + 1
      paramRegisters' = take registerParamCount paramRegisters
      prologue =
        -- We push %rbp separate from the other registers, because it needs to
        -- be saved before we set the frame pointer.
        [ Lined 0 (TwacRStatement $ TwacLabel $ Label $ typeName ++ "." ++ name),
          Lined 0 $ Push Rbp,
          Lined 0 $ TwacRStatement $ Assign Rsp Rbp
        ]
          ++ map (Lined 0 . Push) (tail calleeSavedRegisters)
          ++ map (Lined 0 . Push) paramRegisters'
          ++ [ Lined 0 $ AllocateStackSpace temporarySpace,
               Lined 0 $ Load (ParameterV 0) R15
             ]
      epilogue =
        Lined 0 (DeallocateStackSpace (temporarySpace + length paramRegisters'))
          : map (Lined 0 . Pop) (reverse calleeSavedRegisters)
      -- do not touch internal exceptions, except to give them a label
      twacR = generateTwacR epilogue body
      body' = case twacR of
        [Lined _ (TwacRStatement (TwacInternal _))] -> Lined 0 (TwacRStatement $ TwacLabel $ Label $ typeName ++ "." ++ name) : twacR
        t -> prologue ++ twacR
   in TwacRMethod
        name
        body'
        registerParamCount
        memoryParamCount

generateTwacRIr :: TwacIIr -> TwacRIr
generateTwacRIr (TwacIr impMap typeDetailsMap) =
  TwacRIr
    (Map.mapWithKey (fmap . fmap . generateTwacRMethod) impMap)
    typeDetailsMap
