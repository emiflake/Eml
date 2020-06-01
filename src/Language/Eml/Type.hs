{-# LANGUAGE FlexibleContexts #-}

module Language.Eml.Type where

import Data.Set (Set)
import qualified Data.Set as Set

data Type
  = TyCon String -- String / etc.
  | Type :~> Type -- `a -> b`
  | TyVar String -- `a`
  | TyApp Type Type -- `Foo a`, highly experimental
  | TyForall String Type
  deriving (Eq, Show)

infixr 9 :~>

ftv :: Type -> Set String
ftv ty = case ty of
  TyCon _ -> Set.empty
  a :~> b -> Set.union (ftv a) (ftv b)
  TyVar v -> Set.singleton v
  TyApp _ _ -> Set.empty
  TyForall v t -> Set.delete v (ftv t)

data TypeError
  = UnificationError Type Type -- Two types are not unifiable; e.g. Number ~ a
  | OccursCheck String Type -- Type variable occurs in non-simple type; e.g. a ~ (a -> a)
  | MissingVariable String -- Type variable could not be found in environment

instance Show TypeError where
  show e = case e of
    UnificationError a b -> "Could not unify " <> pretty a <> " ~ " <> pretty b
    OccursCheck v expr -> "Occurs check failed " <> v <> " occurs in " <> pretty expr
    MissingVariable v -> "Type variable " <> v <> " not in scope"

pretty :: Type -> String
pretty ty = case ty of
  TyCon n -> n
  a :~> b ->
    case a of
      TyVar v -> v <> " -> " <> pretty b
      _ -> "(" <> pretty a <> ") -> " <> pretty b
  TyVar v -> v
  TyApp c v -> pretty c <> " " <> pretty v
  TyForall v t -> "forall " <> v <> " . " <> pretty t

stringTy :: Type
stringTy = TyCon "String"

numTy :: Type
numTy = TyCon "Num"

boolTy :: Type
boolTy = TyCon "Bool"

unitTy :: Type
unitTy = TyCon "Unit"
