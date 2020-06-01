module Main where

import Control.Monad
import qualified Data.Map as Map
import Language.Eml.AST as Eml
import Language.Eml.AST as AST
import Language.Eml.Desugar as Eml
import Language.Eml.JavaScript as Eml
import Language.Eml.Parser as Eml
import Language.Eml.TypeChecker as Eml
import System.Environment
import Text.Pretty.Simple

usage = putStrLn "blabla help me"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      ast <- Eml.doParseFile filename
      case ast of
        Left e -> print e
        Right ast' -> do
          let desugared = Eml.desugarModule ast'
          -- pPrint ast
          res <- Eml.checkIO desugared
          case res of
            Left e -> print e
            Right r -> Eml.compileModule r desugared >>= putStrLn
    _ -> usage
