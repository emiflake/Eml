module Language.Eml.AST
  ( Module (..),
    Definition (..),
    Expr (..),
  )
where

import Language.Eml.Operator (Operator (..))
import Language.Eml.Type

data Module
  = Module String [Definition]
  deriving (Show)

data Definition
  = Definition String Expr
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
