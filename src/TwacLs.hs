module TwacLs where

import Trac (Variable)
import Twac

-- Twac Load Store (Generic)
data TwacLsG v = TwacLs (Twac v) | Load !v | Store !v

type TwacLs = TwacLsG Variable
