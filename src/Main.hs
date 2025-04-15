{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Assembly
import Control.Monad (unless, when)
import Data.List (isSuffixOf)
import Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import InputIr
import InputIrParser
import System.Directory.Internal.Prelude (getArgs)
import Trac
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

  let (InputIr classMap _ parentMap _) = inputIr
  -- this is used by generateTrac to make jump tables for case statements
  let pickLowestParents' = pickLowestParents classMap parentMap

  when debug $ putStrLn $ targetClass ++ "." ++ targetMethod ++ " Trac: "
  let (tracIr, temporaryState) = generateTrac pickLowestParents' inputIr
  let (TracIr tracImpMap _) = tracIr
  let TracMethod _ tracBody _ _ = findMethod' Trac.methodName tracImpMap
  when debug $ putStrLn $ showLines tracBody

  when debug $ putStrLn $ targetClass ++ "." ++ targetMethod ++ " Twac: "
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
