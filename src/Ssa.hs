module Ssa where

import Cfg (Cfg)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Trac (TracStatement)
import Util (Label, Variable)

data SsaVariable = SsaVariable Variable Int

generateSsa :: Cfg (TracStatement Variable) Variable -> Cfg (TracStatement SsaVariable) SsaVariable
generateSsa = undefined

dominanceFrontiers :: Cfg s v -> Map.Map Label (Set.Set Label)
dominanceFrontiers = undefined
