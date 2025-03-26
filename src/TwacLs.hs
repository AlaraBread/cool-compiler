module TwacLs where

import Trac (Variable)
import Twac
import Util

-- Twac Load Store (Generic)
type TwacLsG v = [Lined (TwacLsStatementG v)]

type TwacLs = TwacLsG Variable

data TwacLsStatementG v = TwacLsStatement (TwacStatement v) | Load !v | Store !v

type TwacLsStatement = TwacLsStatementG Variable
