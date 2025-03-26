module TwacR where

import TwacLs (TwacLsG)

-- Trac, but with x86-64 register allocation
type TracR = TwacLsG Registers

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
