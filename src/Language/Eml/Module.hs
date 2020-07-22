module Language.Eml.Module
  ( ModulePath (..),
  )
where

import Language.Eml.Emit

newtype ModulePath
  = ModulePath {unwrapModulePath :: [String]}
  deriving (Show, Eq)

instance Emit ModulePath where
  emit (ModulePath path) =
    case path of
      [] -> ""
      [x] -> x
      (x : xs) -> x ++ "$" ++ emit (ModulePath xs)
