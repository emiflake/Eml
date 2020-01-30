module Language.Eml.JavaScript where

import           Language.Eml.AST as A
import Language.Eml.Operator (Operator(..))

compileModule :: A.Module -> IO String
compileModule (A.Module defs) =
  let body = unlines $
        "// Eml compiled module"
        : fmap compileDefinition defs
  in (body++) <$> readFile "marshal/StandardLibrary.js"

compileDefinition :: A.Definition -> String
compileDefinition (A.Definition name expr) = "const " <> name <> " = " <> compileExpr expr <> ";"

compileExpr :: A.Expr -> String
compileExpr (A.NumLit n)   = show n
compileExpr (A.StringLit s) = "\"" <> s <> "\""
compileExpr (A.App f a)    = compileExpr f <> "(" <> compileExpr a <> ")"
compileExpr (A.Lam k body) = "((" <> k <> ") => " <> compileExpr body <> ")"
compileExpr (A.Let rep e b) = "((" <> rep <> ") => (" <> compileExpr b <> "))(" <> compileExpr e <> ")"
compileExpr (A.Var name) = name
compileExpr (A.If cond t f) = "(" <> compileExpr cond <> ") ? (" <> compileExpr t <> ") : (" <> compileExpr f <> ")"
compileExpr (A.BinOp op lhs rhs) =
  let symbol =
        case op of
          Plus -> "+"
          Minus -> "-"
          Multiply -> "*"
  in compileExpr lhs <> " " <> symbol <> " " <> compileExpr rhs


