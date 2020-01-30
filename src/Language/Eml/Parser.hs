module Language.Eml.Parser
  ( Module(..)
  , Definition(..)
  , Expr(..)
  , doParseModule
  , doParseFile
  ) where

import           Control.Monad

import           Text.Parsec
import           Text.Parsec.String

import Language.Eml.Operator (Operator(..))

import Data.List.NonEmpty

data Module
  = Module [Definition]
  deriving Show

data Definition
  = Definition String Expr
  deriving Show

{-| AST specific for parsing.
    Gets desugared later.
-}
data Expr = NumLit Int
          | StringLit String
          | App (NonEmpty Expr)
          | Lam (NonEmpty String) Expr
          | Let String Expr Expr
          | Var String
          | BinOp Operator Expr Expr
          | If Expr Expr Expr
          deriving Show

whitespace :: Parser ()
whitespace = void $ many (oneOf [' ', '\t', '\n'])

identifier :: Parser String
identifier =
  (:) <$> idFirst <*> many idRest
  where idFirst = oneOf (['_'] <> ['A'..'Z'] <> ['a'..'z'])
        idRest = oneOf (['_'] <> ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'])

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

parseModule :: Parser Module
parseModule =
  string "begin" >>
  whitespace >>
  Module <$> many parseDefinition

parseDefinition :: Parser Definition
parseDefinition = do
  name <- identifier
  _ <- whitespace
  _ <- string "="
  _ <- whitespace
  expr <- parseAST
  _ <- whitespace
  pure $ Definition name expr


parseNumLit :: Parser Expr
parseNumLit = NumLit . read <$> many1 digit

parseStringLit :: Parser Expr
parseStringLit = StringLit <$> parseString
  where
    escape :: Parser String
    escape = do
        d <- char '\\'
        c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
        return [d, c]

    nonEscape :: Parser Char
    nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

    character :: Parser String
    character = fmap return nonEscape <|> escape

    parseString :: Parser String
    parseString = do
        _ <- char '"'
        strings <- many character
        _ <- char '"'
        return $ concat strings

parseVar :: Parser Expr
parseVar = Var <$> identifier

parseLam :: Parser Expr
parseLam = do
  let some1 p = let p' = whitespace *> p <* whitespace
                in (:|) <$> p' <*> many p'
  _ <- char '\\'
  _ <- whitespace
  varNames <- some1 identifier
  _ <- whitespace
  _ <- char '.'
  _ <- whitespace
  body <- parseAST
  pure $ Lam varNames body

parseIf :: Parser Expr
parseIf = do
  _ <- string "if"
  _ <- whitespace
  cond <- parseAST
  _ <- whitespace
  _ <- string "then"
  _ <- whitespace
  t <- parseAST
  _ <- whitespace
  _ <- string "else"
  _ <- whitespace
  f <- parseAST
  pure $ If cond t f
parseLet :: Parser Expr
parseLet = do
  _ <- string "let"
  _ <- whitespace
  varName <- identifier
  _ <- whitespace
  _ <- char '='
  _ <- whitespace
  with <- parseAST
  _ <- whitespace
  _ <- string "in"
  _ <- whitespace
  body <- parseAST
  pure $ Let varName with body

parseApp :: Parser Expr
parseApp = atomic <|> (parens $ do
  lhs <- parseAST
  try (parseOp' lhs) <|> (parseApp' lhs <?> "function application"))
  where
    parseOp' lhs = do
      _ <- whitespace
      op <- parseOpSymbol
      _ <- whitespace
      rhs <- parseAST
      pure $ BinOp op lhs rhs
    parseApp' lhs = do
      _ <- whitespace
      rhs <- many (whitespace *> parseAST <* whitespace)
      pure $ App (lhs :| rhs)
    atomic = parseNumLit <|> parseVar <|> parseStringLit
    parseOpSymbol = (Plus <$ char '+') <|> (Minus <$ char '-' ) <|> (Multiply <$ char '*')

parseAST :: Parser Expr
parseAST =
       parseIf
       <|> parseApp
       <|> parseLet
       <|> parseLam

doParseModule :: String -> Either ParseError Module
doParseModule = parse parseModule ""

doParseFile :: FilePath -> IO (Either ParseError Module)
doParseFile path = doParseModule <$> readFile path
