module Main where

import InputIrParser
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- readFile inputFile
  let ast = InputIrParser.parse input
  print ast
