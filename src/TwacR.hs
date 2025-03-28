module TwacR where

import Control.Exception (assert)
import qualified Data.Map as Map
import qualified Data.Set as Set
import InputIr (Formal, Type)
import Trac (Variable)
import Twac
import Util

data TwacRIr = TwacRIr {implementationMap :: Map.Map Type [TwacRMethod], constructorMap :: Map.Map Type TwacR}

data TwacRMethod = TwacRMethod {methodName :: String, body :: TwacR, formals :: [Formal]}

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
  deriving (Show, Eq, Ord)

allRegisters = Set.fromList [Rax, Rbx, Rcx, Rdx, Rsi, Rsp, Rbp, R8, R9, R10, R11, R12, R13, R14, R15]

paramRegisters = [Rdi, Rsi, Rdx, Rcx, R8, R9]

calleeSavedRegisters = [Rbp, Rbx, Rsp, R12, R13, R14, R15]

-- This is a really inefficient way of doing this, at least for Rax and Rdx.
-- Anyways...
reservedRegisters =
  Set.fromList
    [ Rsp, -- stack pointer
      Rbp, -- frame pointer
      Rax, -- needed for division
      Rdx -- needed for division
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
        Divide src dst ->
          let (srcR, freeRegisters') = Set.deleteFindMin freeRegisters
           in [ Load dst Rax,
                Load src srcR,
                -- in the code gen stage, this pretty much is just cqto; idiv srcR
                TwacRStatement $ Divide srcR Rax,
                Store Rax dst
              ]
        LessThan src dst -> binaryOperation LessThan src dst
        LessThanOrEqualTo src dst -> binaryOperation LessThanOrEqualTo src dst
        Equals src dst -> binaryOperation Equals src dst
        IntConstant i v -> immediate (IntConstant i) v
        BoolConstant i v -> immediate (BoolConstant i) v
        StringConstant i v -> immediate (StringConstant i) v
        Not dst -> unaryOperation Not dst
        Negate dst -> unaryOperation Not dst
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
          map item epilogue ++ [
            Load ret Rax,
            TwacRStatement $ Return Rax
          ]
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
        TwacCase v jmpTable ->
          let (vR, freeRegisters') = Set.deleteFindMin freeRegisters
           in [ Load v vR,
                TwacRStatement $ TwacCase vR jmpTable
              ]
        Abort line message -> [TwacRStatement $ Abort line message]

generateTwacR :: TwacR -> TwacI -> TwacR
generateTwacR epilogue = concatMap (unsequence . fmap (generateTwacRStatements epilogue freeRegisters))

generateTwacRMethod :: TwacMethod Variable -> TwacRMethod
generateTwacRMethod (TwacMethod name body formals temporaryCount) =
  let -- This is always a multiple of 16 bytes, to keep the stack 16-byte aligned.
      temporarySpace = temporaryCount + (temporaryCount + length paramRegisters) `rem` 1
      prologue =
        map (Lined 0 . Push) calleeSavedRegisters
          ++ map (Lined 0 . Push) paramRegisters
          ++ [Lined 0 $ AllocateStackSpace temporarySpace]
      epilogue =
        Lined 0 (DeallocateStackSpace (temporarySpace + length paramRegisters))
          : map (Lined 0 . Pop) (reverse calleeSavedRegisters)
   in TwacRMethod
        name
        (prologue ++ generateTwacR epilogue body)
        formals

generateTwacRIr :: TwacIIr -> TwacRIr
generateTwacRIr (TwacIr impMap constructorMap) =
  let gen = generateTwacRStatements [] freeRegisters
   in TwacRIr (fmap (fmap generateTwacRMethod) impMap) (fmap (generateTwacR []) constructorMap)
