module Main where

import Assembly (generateAssembly, sanitizeString)
import Cfg (CfgIr (CfgIr), CfgMethod (..), cfgMethodToLinearCode, cfgToGraphviz, cfgToLinearCode, createCfgIr, mapCfgIr, mapCfgMethod)
import ConstantFolding (constantFold)
import Control.Monad (unless, when)
import Data.List (isSuffixOf)
import Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import DeadCodeElimination (deadCodeElimination)
import Debug.Trace (traceShowM)
import InputIr
import InputIrParser
import Interpreter (interpret)
import Ssa (dropSsa, generateSsa)
import System.Directory.Internal.Prelude (getArgs)
import System.Timeout (timeout)
import Trac
import TracIr (TracIr (TracIr), TracMethod (TracMethod))
import qualified TracIr
import Twac
import TwacR
import Util

-- Takes an input file name and gives us an output file name.
outputFile :: String -> String
outputFile input = reverse $ "s" ++ Prelude.drop 7 (reverse input)

findMethod :: String -> String -> (m -> String) -> Map Type [ImplementationMapEntry m] -> m
findMethod targetClass targetMethod name implMap =
  head $
    Prelude.filter (\m -> name m == targetMethod) $
      Data.Maybe.mapMaybe implementationMapEntryToMaybe (implMap Map.! Type targetClass)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  unless (".cl-type" `isSuffixOf` inputFile) $ error "Input file must end in .cl-type."
  let debug = (length args > 3) && (args !! 1 == "--debug")

  let targetClass = args !! 2
  let targetMethod = args !! 3
  let findMethod' = findMethod targetClass targetMethod

  input <- readFile inputFile
  let inputIr = InputIrParser.parse input

  -- we timeout writing the sanitized string, as that is a concrete "thing" to do that we can actually timeout
  let result = interpret inputIr
  -- True for success, False for failure
  let writeInterpretedResult :: IO Bool
      writeInterpretedResult = case result of
        Just res ->
          writeFile
            (outputFile inputFile)
            ( ".section .note.GNU-stack, \"\", @progbits\n\n.section .data\nformat: .string \"%s\"\ncomputed: .string \""
                ++ sanitizeString res
                ++ "\"\n.section .text\n.globl main\nmain:\nsubq $8, %rsp\nleaq computed, %rsi\nleaq format, %rdi\ncall printf\naddq $8, %rsp\nret\n"
            )
            *> pure True
        Nothing -> pure False

  status <- writeInterpretedResult

  -- fine, we'll *actually* compile it, I guess.
  unless status $ do
    let (InputIr classMap _ parentMap _) = inputIr
    -- this is used by generateTrac to make jump tables for case statements
    let pickLowestParents' = pickLowestParents classMap parentMap

    when debug $ putStrLn $ targetClass ++ "." ++ targetMethod ++ " Trac: "
    let (tracIr, temporaryState) = generateTrac pickLowestParents' inputIr
    let (TracIr tracImpMap _) = tracIr
    let TracMethod _ tracBody _ _ = findMethod' TracIr.methodName tracImpMap
    when debug $ putStrLn $ showLines tracBody

    let tracCfg = createCfgIr tracIr
    let (CfgIr cfgImpMap _) = tracCfg
    when debug $ putStrLn $ targetClass ++ "." ++ targetMethod ++ " Cfg Trac: "
    let CfgMethod _ cfgBody _ _ = findMethod' Cfg.methodName cfgImpMap
    when debug $ print cfgBody
    when debug $ putStrLn $ cfgToGraphviz cfgBody

    let ssaCfg = mapCfgIr generateSsa tracCfg
    let (CfgIr ssaImpMap _) = ssaCfg
    -- when debug $ putStrLn $ targetClass ++ "." ++ targetMethod ++ " Cfg Ssa: "
    let CfgMethod _ ssaBody _ _ = findMethod' Cfg.methodName ssaImpMap
    -- when debug $ print ssaBody
    -- when debug $ putStrLn $ cfgToGraphviz ssaBody

    -- let fix f x =
    --       if x == f x
    --         then x
    --         else fix f (f x)
    -- let ssaCfgOptimized = mapCfgIr (fix (deadCodeElimination . constantFold)) ssaCfg

    -- -- converting back to boring, straight-line code. personally I prefer
    -- -- gay-line code but whatever.
    -- let CfgIr cfgImpMap' cfgTypeDetailsMap' = mapCfgIr dropSsa ssaCfgOptimized
    -- let tracIr' = TracIr (Map.map (fmap (fmap cfgMethodToLinearCode)) cfgImpMap') cfgTypeDetailsMap'

    when debug $ putStrLn $ targetClass ++ "." ++ targetMethod ++ " Twac: "

    -- notably we do not use tracIr', i.e. the optimized version. unfortunately,
    -- our cfg construction is mildly buggy which completely destroys our
    -- correctness and makes that not worthwhile :<
    let (twacIr, temporaryState') = generateTwac tracIr temporaryState
    let TwacIr twacImpMap _ = twacIr
    let TwacMethod _ twacBody _ _ = findMethod' Twac.methodName twacImpMap
    when debug $ putStrLn $ showLines twacBody

    when debug $ putStrLn $ targetClass ++ "." ++ targetMethod ++ " TwacR: "
    let twacRIr = generateTwacRIr twacIr
    let TwacRIr twacRImpMap _ = twacRIr
    let TwacRMethod _ twacRBody _ _ = findMethod' TwacR.methodName twacRImpMap
    when debug $ putStrLn $ showLines twacRBody

    -- when debug $ putStrLn "asm: "
    let asmIr = generateAssembly temporaryState' twacRIr
    -- when debug $ print asmIr
    writeFile (outputFile inputFile) $ show asmIr
