module Language.Eml.Desugar where

import           Language.Eml.AST      as A
import           Language.Eml.Operator
import           Language.Eml.Parser   as P

import           Data.List.NonEmpty

desugarModule :: P.Module -> A.Module
desugarModule (P.Module name defs) = A.Module name (fmap desugarDef defs)

desugarDef :: P.Definition -> A.Definition
desugarDef (P.Definition name expr) = A.Definition name (desugarExpr expr)

desugarExpr :: P.Expr -> A.Expr
desugarExpr (P.NumLit num) = A.NumLit num
desugarExpr (P.StringLit str) = A.StringLit str
desugarExpr (P.ListLit xs) = foldr (A.BinOp Cons) (A.Var "nil") (desugarExpr <$> xs)
desugarExpr (P.App app) = foldl1 A.App (desugarExpr <$> app)
desugarExpr (P.Lam (h :| t) e) = foldr A.Lam (desugarExpr e) (h : t)
desugarExpr (P.Let k v e) = A.Let k (desugarExpr v) (desugarExpr e)
desugarExpr (P.Var v) = A.Var v
desugarExpr (P.BinOp op lhs rhs) = A.BinOp op (desugarExpr lhs) (desugarExpr rhs)
desugarExpr (P.If cond t f) = A.If (desugarExpr cond) (desugarExpr t) (desugarExpr f)
desugarExpr (P.Asc expr ty) = A.Asc (desugarExpr expr) ty


