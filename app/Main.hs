module Main where

import           System.Environment

import           Language.Eml.AST         as Eml
import           Language.Eml.AST         as AST
import           Language.Eml.Desugar     as Eml
import           Language.Eml.JavaScript  as Eml
import           Language.Eml.Parser      as Eml
import           Language.Eml.TypeChecker as Eml

import           Control.Monad
import qualified Data.Map                 as Map
import           Text.Pretty.Simple

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
          pPrint ast
          res <- Eml.checkIO desugared
          case res of
            Left e  -> print e
            Right r ->
              do
                Eml.compileModule desugared >>= putStrLn
                forM_ (Map.assocs r) $ \(k, v) -> do
                  putStrLn $ k <> " : " <> show v
    _ -> usage
