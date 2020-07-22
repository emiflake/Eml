module Language.Eml.JavaScript where

import Data.Map (Map)
import qualified Data.Map as Map
import Language.Eml.AST as A
import Language.Eml.Emit
import Language.Eml.Operator (Operator (..))
import Language.Eml.Type (Type)

compileModule :: Map String Type -> A.Module -> IO String
compileModule env (A.Module path defs) =
  let body =
        unlines $
          "/*"
            : "** Eml compiled module"
            : "** " <> emit path <> ".eml"
            : "*/"
            : fmap (compileDefinition env) defs
   in pure $ body <> "\nmain();"

escapeQuot = fmap (\c -> if c == '\'' then '$' else c)

compileDefinition :: Map String Type -> A.TopLevelDefinition -> String
compileDefinition env (A.TermDefinition name expr) =
  case Map.lookup name env of
    Just ty ->
      "// " <> name <> " : " <> emit ty <> "\n"
        <> "const "
        <> escapeQuot name
        <> " = "
        <> compileExpr expr
        <> ";"
    Nothing ->
      "const " <> escapeQuot name <> " = " <> compileExpr expr <> ";"

compileExpr :: A.Expr -> String
compileExpr (A.NumLit n) = show n
compileExpr (A.StringLit s) = "\"" <> s <> "\""
compileExpr (A.App f a) =
  case (f, a) of
    (A.Var "eval", A.StringLit s) -> s
    (_, _) -> compileExpr f <> "(" <> compileExpr a <> ")"
compileExpr (A.Lam k body) = "(" <> k <> " => " <> compileExpr body <> ")"
-- Hacky hack.
-- TODO: figure out how to do proper recursion, somehow. Or maybe not.
compileExpr (A.Let rep e b) =
  "((" <> escapeQuot rep <> ") => ("
    <> compileExpr b
    <> "))((()=>{const "
    <> escapeQuot rep
    <> " = "
    <> compileExpr e
    <> ";return "
    <> escapeQuot rep
    <> "})())"
compileExpr (A.Var name) = escapeQuot name
compileExpr (A.If cond t f) = "(" <> compileExpr cond <> ") ? (" <> compileExpr t <> ") : (" <> compileExpr f <> ")"
compileExpr (A.BinOp Cons lhs rhs) = "(cons(" <> compileExpr lhs <> ")(" <> compileExpr rhs <> "))"
compileExpr (A.Asc expr _) = compileExpr expr
compileExpr (A.BinOp op lhs rhs) =
  let symbol =
        case op of
          Plus -> "+"
          Minus -> "-"
          Multiply -> "*"
          _ -> undefined
   in compileExpr lhs <> " " <> symbol <> " " <> compileExpr rhs
