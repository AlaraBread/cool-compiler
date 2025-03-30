{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Assembly
import Data.Map.Strict as Map
import InputIr
import InputIrParser
import System.Directory.Internal.Prelude (getArgs, when)
import Trac
import Twac
import TwacR

-- Takes an input file name and gives us an output file name.
outputFile :: String -> String
outputFile input = reverse $ "s" ++ Prelude.drop 7 (reverse input)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  let debug = (length args > 1) && (args !! 1 == "--debug")

  input <- readFile inputFile
  let inputIr = InputIrParser.parse input

  -- this is used by generateTwac to make jump tables for case statements
  let (InputIr classMap _ parentMap _) = inputIr
  let pickLowestParents' = pickLowestParents classMap parentMap

  when debug $ putStrLn "Main.main Trac: "
  let (tracIr, temporaryState) = generateTrac inputIr
  let (TracIr tracImpMap _ _) = tracIr
  let mainClassMethods = tracImpMap Map.! Type "Main"
  let TracMethod _ body _ _ =
        head $ Prelude.filter (\m -> Trac.methodName m == "main") mainClassMethods
  when debug $ putStrLn $ showTrac body

  when debug $ putStrLn "Main.main Twac: "
  let (twacIr, temporaryState') = generateTwac pickLowestParents' tracIr temporaryState
  let TwacIr twacImpMap _ _ = twacIr
  let mainClassMethods = twacImpMap Map.! Type "Main"
  let TwacMethod _ body _ _ =
        head $ Prelude.filter (\m -> Twac.methodName m == "main") mainClassMethods
  when debug $ putStrLn $ showTwac body

  when debug $ putStrLn "Main.main TwacR: "
  let twacRIr = generateTwacRIr twacIr
  let TwacRIr twacRImpMap _ _ = twacRIr
  let mainClassMethods = twacRImpMap Map.! Type "Main"
  let TwacRMethod _ body _ _ =
        head $ Prelude.filter (\m -> TwacR.methodName m == "main") mainClassMethods
  when debug $ putStrLn $ showTwac body

  when debug $ putStrLn "asm: "
  let asmIr = generateAssembly temporaryState' twacRIr
  when debug $ print asmIr
  writeFile (outputFile inputFile) $ show asmIr
