module TwacR where

import Trac (Variable)
import Twac
import Util

-- Twac with Register allocation, and consequently load/store operations.
type TwacR = [Lined TwacRStatement]

data TwacRStatement
  = TwacRStatement (TwacStatement Register)
  | Load Variable Register
  | Store Register Variable

data Register
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
