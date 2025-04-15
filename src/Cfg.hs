type Ssa = Trac SsaVariable

data Cfg = Cfg
  { cfgChildren :: Map.Map Label (Set Label),
    cfgBlocks :: Map.Map Label [Ssa]
  }

-- this will reverse the edges and also reverse the statements in cfgBlocks
reverseCfg :: Cfg -> Cfg
reverseCfg = undefined

class Lattice a where
  top :: a
  bottom :: a
  meet :: a -> a -> a
  join :: a -> a -> a -- aka lub

instance PartialOrd (Lattice a) where
  (<=) = (join a b) == b

class (Lattice a) => Ai a where
  addTf :: a -> a -> a

-- ......

runAi :: (Ai a) => Cfg -> Map SsaVariable a
