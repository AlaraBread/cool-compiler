{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Map.Strict as Map
import InputIr
import InputIrParser
import System.Directory.Internal.Prelude (getArgs)
import Trac
import Twac

-- Takes an input file name and gives us an output file name.
outputFile :: String -> String
outputFile input = reverse $ outputFile' $ reverse input
  where
    outputFile' (_ : _ : _ : _ : rest) = "cawt" ++ rest -- meow :3

main :: IO ()
main = do
  inputFile <- head <$> getArgs
  input <- readFile inputFile
  let inputIr = InputIrParser.parse input

  let (tracIr, temporaryState) = generateTrac inputIr

  -- this is used by generateTwac to make jump tables for case statements
  let (InputIr classMap _ parentMap _) = inputIr
  let pickLowestParents' = pickLowestParents classMap parentMap

  let (TwacIr twacImpMap _ _, temporaryState') = generateTwac pickLowestParents' tracIr temporaryState


  putStrLn "Main.main Trac: "
  let (TracIr tracImpMap _ _) = tracIr
  let mainClassMethods' = tracImpMap Map.! Type "Main"
  let TracMethod _ body' _ _ =
        head $ Prelude.filter (\m -> Trac.methodName m == "main") mainClassMethods'
  putStrLn $ showTrac body'

  -- We want to find the Main method
  let mainClassMethods = twacImpMap Map.! Type "Main"

  putStrLn "Main.main Twac: "
  let TwacMethod _ body _ _ =
        head $ Prelude.filter (\m -> Twac.methodName m == "main") mainClassMethods
  putStrLn $ showTwac body
