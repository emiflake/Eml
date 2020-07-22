module Language.Eml.AST
  ( Module (..),
    TopLevelDefinition (..),
    Expr (..),
  )
where

import Language.Eml.Module
import Language.Eml.Operator (Operator (..))
import Language.Eml.Type

data Module
  = Module ModulePath [TopLevelDefinition]
  deriving (Show)

data TopLevelDefinition
  = TermDefinition String Expr
  deriving (Show)

data Expr
  = NumLit Int
  | StringLit String
  | Asc Expr Type
  | App Expr Expr
  | Lam String Expr
  | Let String Expr Expr
  | Var String
  | BinOp Operator Expr Expr
  | If Expr Expr Expr
  deriving (Show)
