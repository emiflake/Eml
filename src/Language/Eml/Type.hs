{-# LANGUAGE FlexibleContexts #-}
module Language.Eml.Type where

import           Data.Set (Set)
import qualified Data.Set as Set

data Type
  = NumType -- `Number`
  | BoolType -- `Bool`
  | StringType -- `String`
  | Type :~> Type -- `a -> b`
  | TyVar String -- `a`
  | TyForall String Type
  deriving (Eq, Show)

infixr :~>

ftv :: Type -> Set String
ftv ty = case ty of
  NumType      -> Set.empty
  BoolType     -> Set.empty
  StringType   -> Set.empty
  a :~> b      -> Set.union (ftv a) (ftv b)
  TyVar v      -> Set.singleton v
  TyForall v t -> Set.delete v (ftv t)

data TypeError
  = UnificationError Type Type -- Two types are not unifiable; e.g. Number ~ a
  | OccursCheck String Type    -- Type variable occurs in non-simple type; e.g. a ~ (a -> a)
  | MissingVariable String     -- Type variable could not be found in environment


instance Show TypeError where
  show e = case e of
    UnificationError a b -> "Could not unify " <> show a <> " ~ " <> show b
    OccursCheck v expr -> "Occurs check failed " <> v <> " occurs in " <> show expr
    MissingVariable v -> "Type variable " <> v <> " not in scope"

