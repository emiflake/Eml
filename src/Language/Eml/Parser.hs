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

import           Language.Eml.Operator (Operator (..))

import           Data.List.NonEmpty    hiding (some1)

data Module
  = Module [Definition]
  deriving Show

data Definition
  = Definition String Expr
  deriving Show

doParseModule :: String -> Either ParseError Module
doParseModule = parse module' ""

doParseFile :: FilePath -> IO (Either ParseError Module)
doParseFile path = doParseModule <$> readFile path

module' :: Parser Module
module' =
  string "begin" >>
  whitespace' >>
  Module <$> many1 definition

definition :: Parser Definition
definition = do
  name <- identifier
  _ <- whitespace'
  _ <- string "="
  _ <- whitespace'
  v <- expr
  _ <- whitespace'
  pure $ Definition name v


{-| Grammar

lit = :numlit    #number
    | :stringlit #string

op = :operator #op

exp = :lit         lit
    | :application exp exp
    | :binop       exp op exp
    | :parentheses "(" exp ")"
    | :let         "let" var "=" exp "in" exp
    | :if          "if" exp "then" exp "else" exp
    | :lam         "\" var "." exp

-}

{-| AST specific for parsing.
    Gets desugared later.
-}
data Expr = NumLit Int
          | StringLit String
          | ListLit [Expr]
          | App (NonEmpty Expr)
          | Lam (NonEmpty String) Expr
          | Let String Expr Expr
          | Var String
          | BinOp Operator Expr Expr
          | If Expr Expr Expr
          deriving Show


some1 :: Parser a -> Parser (NonEmpty a)
some1 p = let p' = whitespace *> p <* whitespace
          in (:|) <$> p' <*> many p'

expr :: Parser Expr
expr = App <$> some1 expr' <|> expr'

expr' :: Parser Expr
expr' = do
  lhs <- expr''
  try (binop lhs) <|> pure lhs

expr'' :: Parser Expr
expr'' = (lit <?> "lit")
   <|> (parentheses <?> "parentheses")
   <|> (Var <$> identifier <?> "identifier")
   <|> (if' <?> "if")
   <|> (lam <?> "lambda")

followLine :: Parser ()
followLine =
  void $ many1 (char '\n' >> many1 (char ' '))

{- lit -}
lit :: Parser Expr
lit = numLit <|> stringLit

numLit :: Parser Expr
numLit = NumLit . read <$> many1 digit

stringLit :: Parser Expr
stringLit = StringLit <$> parseString
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

{- binop -}
binop :: Expr -> Parser Expr
binop lhs =
  let op = (Plus     <$ char '+')
       <|> (Minus    <$ char '-')
       <|> (Multiply <$ char '*')
       <|> (Cons     <$ string "::")
  in whitespaced op     >>= \op' ->
     whitespaced expr' >>= \rhs -> pure $ BinOp op' lhs rhs

{- parentheses -}
parentheses :: Parser Expr
parentheses = whitespaced $
  char '(' *> (whitespaced expr) <* char ')'

{- if -}
if' :: Parser Expr
if' = do
  If <$  string "if"
     <*> whitespaced expr
     <*  string "then"
     <*> whitespaced expr
     <*  string "else"
     <*> whitespaced expr

{- lam -}
lam :: Parser Expr
lam = do
  let some1 p = let p' = whitespace *> p <* whitespace
                in (:|) <$> p' <*> many p'
  _ <- char '\\'
  _ <- whitespace
  varNames <- some1 identifier
  _ <- whitespace
  _ <- char '.'
  _ <- whitespace
  body <- expr
  pure $ Lam varNames body

{- util -}
indent :: Parser ()
indent = void $ oneOf [' ', '\t']

whitespace :: Parser ()
whitespace = do
  _ <- many indent
  -- _ <- many (char '\n' >> indent)
  pure ()

whitespace' :: Parser ()
whitespace' = void . try $ many (oneOf [' ', '\t', '\n'])

whitespaced :: Parser a -> Parser a
whitespaced p = whitespace *> p <* whitespace

identifier :: Parser String
identifier = do
  let sym =
        (:) <$> idFirst <*> many idRest
        where idFirst = oneOf (['_'] <> ['A'..'Z'] <> ['a'..'z'])
              idRest = oneOf (['_', '\''] <> ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'])
  s <- sym
  guard (s `notElem` ["if", "then", "else"])
  pure s


-- parseListLit :: Parser Expr
-- parseListLit = do
--   let elem = whitespace *> parseAST <* whitespace
--   _ <- char '['
--   _ <- whitespace
--   elems <- elem `sepBy` char ','
--   _ <- whitespace
--   _ <- char ']'
--   pure $ ListLit elems

-- whitespace :: Parser ()
-- whitespace = void $ many (oneOf [' ', '\t', '\n'])


-- parens :: Parser a -> Parser a
-- parens p = char '(' *> p <* char ')'



-- parseVar :: Parser Expr
-- parseVar = Var <$> identifier


-- parseLet :: Parser Expr
-- parseLet = do
--   _ <- string "let"
--   _ <- whitespace
--   varName <- identifier
--   _ <- whitespace
--   _ <- char '='
--   _ <- whitespace
--   with <- parseAST
--   _ <- whitespace
--   _ <- string "in"
--   _ <- whitespace
--   body <- parseAST
--   pure $ Let varName with body

-- parseApp :: Parser Expr
-- parseApp = atomic <|> do
--   lhs <- parseAST
--   try (parseOp' lhs) <|> (parseApp' lhs <?> "function application")
--   where
--     parseOp' lhs = do
--       _ <- whitespace
--       op <- parseOpSymbol
--       _ <- whitespace
--       rhs <- parseAST
--       pure $ BinOp op lhs rhs
--     parseApp' lhs = do
--       _ <- whitespace
--       rhs <- many (whitespace *> parseAST <* whitespace)
--       pure $ App (lhs :| rhs)
--     atomic = parseListLit <|> parseNumLit <|> parseVar <|> parseStringLit
--     parseOpSymbol = (Plus <$ char '+')
--                     <|> (Minus <$ char '-' )
--                     <|> (Multiply <$ char '*')
--                     <|> (Cons <$ string "::")

