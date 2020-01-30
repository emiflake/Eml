module Main where

import System.Environment

import Language.Eml.AST as Eml
import Language.Eml.Desugar as Eml
import Language.Eml.JavaScript as Eml
import Language.Eml.Parser as Eml

usage = putStrLn "blabla help me"
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      ast <- Eml.doParseFile filename
      case ast of
        Left e -> print e
        Right ast' ->
          Eml.compileModule (Eml.desugarModule ast') >>= putStrLn
    _ -> usage
