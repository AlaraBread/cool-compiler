{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Cfg where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Util

data Cfg s = Cfg
  { cfgChildren :: Map.Map Label (Set.Set Label),
    cfgBlocks :: Map.Map Label [s]
  }

-- this will reverse the edges and also reverse the statements in cfgBlocks
reverseCfg :: Cfg s -> Cfg s
reverseCfg = undefined

class (Eq a) => Lattice a where
  top :: a
  bottom :: a
  meet :: a -> a -> a
  join :: a -> a -> a -- aka lub
  (<=) :: a -> a -> Bool
  (<=) a b = join a b == b

-- s: statement type
-- v: underlying variable type
-- a: abstract value type (Lattice)

type TransferFunction s v a = (Lattice a) => Map.Map v a -> s -> Map.Map v a

class (Lattice a) => Ai s v a where
  tf :: TransferFunction s v a

-- ......

runAi :: (Ai s v a) => Cfg s -> Map.Map v a
runAi = undefined
