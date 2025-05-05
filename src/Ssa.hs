{-# LANGUAGE NamedFieldPuns #-}

module Ssa where

import Cfg (Cfg (..))
import Control.Monad.State
import Data.Foldable (Foldable (foldl'), find)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Set as Set
import Trac (TracStatement)
import Util (Label, Variable, reverseMap)

data SsaVariable = SsaVariable Variable Int

generateSsa :: Cfg (TracStatement Variable) Variable -> Cfg (TracStatement SsaVariable) SsaVariable
generateSsa cfg =
  let domFrontiers = dominanceFrontiers cfg
      Cfg {cfgStart, cfgBlocks, cfgChildren, cfgPredecessors, cfgVariables, cfgDefinitions} = cfg
      domTree = Map.insert cfgStart Set.empty (reverseMap $ Map.map Set.singleton $ idom cfg)
      revVariableDefinitions = reverseMap cfgDefinitions
      phiFunctions = calculatePhiFunctions domFrontiers revVariableDefinitions
      cfgBlocks' = undefined
      cfgVariables' = undefined
      cfgDefinitions' = undefined
   in Cfg cfgStart cfgBlocks' cfgChildren cfgPredecessors cfgVariables' cfgDefinitions'

calculatePhiFunctions ::
  Map.Map Label (Set.Set Label) ->
  Map.Map Variable (Set.Set Label) ->
  Map.Map Label (Set.Set Variable)
calculatePhiFunctions domFrontiers =
  Map.foldlWithKey'
    ( \p variable blocks ->
        foldl'
          ( \p' block ->
              foldl'
                ( \p'' k ->
                    Map.insert
                      k
                      (Set.insert variable $ fromMaybe Set.empty $ Map.lookup block p'')
                      p''
                )
                p'
                (domFrontiers Map.! block)
          )
          p
          blocks
    )
    Map.empty

-- https://ics.uci.edu/~yeouln/course/ssa.pdf
rename ::
  Label ->
  Map.Map Label (Set.Set Variable) ->
  Map.Map Label (Set.Set Label) ->
  Map.Map Variable SsaVariable ->
  Map.Map Label (Map.Map SsaVariable (Set.Set SsaVariable)) ->
  State (Map.Map Variable Int) (Map.Map Label (Map.Map SsaVariable (Set.Set SsaVariable)))
rename block phiFunctionVariables domTree currentDefinitions phiFunctions =
  do
    let phiFunctionVariablesForBlock = phiFunctionVariables Map.! block
    phiFunctionsForBlock <-
      traverse
        ( \v -> do
            v' <- genName v

            pure undefined
        )
        phiFunctionVariablesForBlock
    let phiFunctions' = Map.insert block phiFunctionsForBlock phiFunctions
    pure phiFunctions'

genName :: Variable -> State (Map.Map Variable Int) SsaVariable
genName = undefined

-- Cooper, Keith D., Harvey, Timothy J. and Kennedy, Ken. "A simple, fast dominance algorithm."
dominanceFrontiers :: Cfg s v -> Map.Map Label (Set.Set Label)
dominanceFrontiers cfg =
  let idoms = idom cfg
   in Map.foldlWithKey'
        ( \frontiers b bPredecessors ->
            if length bPredecessors >= 2
              then
                foldl'
                  (\frontiers' p -> dominanceFrontiers' idoms p b frontiers')
                  frontiers
                  bPredecessors
              else frontiers
        )
        Map.empty
        (cfgPredecessors cfg)

-- from https://en.wikipedia.org/wiki/Static_single-assignment_form
dominanceFrontiers' :: Map.Map Label Label -> Label -> Label -> Map.Map Label (Set.Set Label) -> Map.Map Label (Set.Set Label)
dominanceFrontiers' idoms runner b frontiers =
  if runner == fromJust (Map.lookup b idoms)
    then frontiers
    else
      dominanceFrontiers'
        idoms
        (idoms Map.! runner)
        b
        ( Map.insert
            runner
            ( Set.insert b $
                fromMaybe Set.empty $
                  Map.lookup runner frontiers
            )
            frontiers
        )

-- Cooper, Keith D., Harvey, Timothy J. and Kennedy, Ken. "A simple, fast dominance algorithm."
-- calculate immidiate dominators of all nodes
idom :: Cfg s v -> Map.Map Label Label
idom (Cfg {cfgStart, cfgPredecessors}) =
  let ordering = dfs cfgPredecessors cfgStart
      ordering' = case ordering of
        (_ : o) -> o
        [] -> []
      ordering'' = reverse ordering'
      orderingMap = Map.fromList [(n, i) | n <- ordering'', i <- [0 :: Int ..]]
   in idom' cfgPredecessors ordering'' orderingMap True (Map.singleton cfgStart cfgStart)

idom' ::
  Map.Map Label (Set.Set Label) ->
  [Label] ->
  Map.Map Label Int ->
  Bool ->
  Map.Map Label Label ->
  Map.Map Label Label
idom' predecessors nodes nodeOrdering changed idoms
  | not changed = idoms
  | otherwise =
      let (idoms', changed') =
            foldl'
              ( \(idoms'', changed'') b ->
                  let preds = predecessors Map.! b
                      firstPred = fromJust $ find (\i -> isJust $ Map.lookup i idoms'') preds
                      newIdom =
                        foldl'
                          ( \newIdom' p ->
                              if isJust $ Map.lookup p idoms''
                                then intersect idoms'' nodeOrdering p newIdom'
                                else newIdom'
                          )
                          firstPred
                          preds
                   in if Map.lookup b idoms'' == Just newIdom
                        then (idoms'', changed'')
                        else (Map.insert b newIdom idoms'', True)
              )
              (idoms, False)
              nodes
       in idom' predecessors nodes nodeOrdering changed' idoms'

intersect :: Map.Map Label Label -> Map.Map Label Int -> Label -> Label -> Label
intersect idoms ordering f1 f2
  | ordering Map.! f1 == ordering Map.! f2 = f1
  | ordering Map.! f1 < ordering Map.! f2 = intersect idoms ordering (idoms Map.! f1) f2
  | otherwise = intersect idoms ordering f1 (idoms Map.! f2)

-- depth first ordering
dfs :: Map.Map Label (Set.Set Label) -> Label -> [Label]
dfs edges start = dfs' edges [start] Set.empty

dfs' :: Map.Map Label (Set.Set Label) -> [Label] -> Set.Set Label -> [Label]
dfs' edges (current : rest) visited =
  let new = Set.difference (fromJust $ Map.lookup current edges) visited
      new' = Set.toList new
      visited' = visited <> new
   in current
        : Set.toList new
        ++ dfs' edges (rest ++ new') visited'
dfs' _ [] _ = []
