module TwacR where

import TwacLs (TwacLsG, TwacLsStatementG)
import Util

type TracR = TwacLs Registers

type TwacRStatement = TwacLsStatementG Registers

type TwacLs v = [Lined (TwacLsStatementG v)]

data Registers
  = Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
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
