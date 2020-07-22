module Language.Eml.Emit
  ( Emit (..),
  )
where

class Emit a where
  emit :: a -> String
