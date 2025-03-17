module Main where

import Data.Map.Strict as Map
import InputIr
import InputIrParser
import System.Directory.Internal.Prelude (getArgs)
import Tac

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- readFile inputFile
  let ast = InputIrParser.parse input
  let (TacIr tacImpMap _) = generateTac ast
  -- We want to find the Main method
  let mainClassMethods = tacImpMap Map.! Type "Main"
  let TacMethod {body} =
        head $ Prelude.filter (\m -> Tac.methodName m == "main") mainClassMethods
  putStrLn $ showTac body
