{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Map.Strict as Map
import InputIr
import InputIrParser
import System.Directory.Internal.Prelude (getArgs)
import Trac

-- Takes an input file name and gives us an output file name.
outputFile :: String -> String
outputFile input = reverse $ outputFile' $ reverse input
  where
    outputFile' (_ : _ : _ : _ : rest) = "cat" ++ rest -- meow :3

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- readFile inputFile
  let ast = InputIrParser.parse input
  let (TracIr tracImpMap _) = generateTrac ast
  -- We want to find the Main method
  let mainClassMethods = tracImpMap Map.! Type "Main"
  let TracMethod {body} =
        head $ Prelude.filter (\m -> Trac.methodName m == "main") mainClassMethods
  writeFile (outputFile inputFile) $ showTrac body
