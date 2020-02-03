module Language.Eml.Parser
  ( Module(..)
  , Definition(..)
  , Expr(..)
  , doParseModule
  , doParseFile
  ) where

import           Control.Applicative   (Alternative)
import           Control.Monad

import           Text.Parsec
import           Text.Parsec.String

import           Language.Eml.Operator (Operator (..))
import           Language.Eml.Type

import           Data.List.NonEmpty    hiding (some1)

data Module
  = Module String [Definition]
  deriving Show

data Definition
  = Definition
    { _bindingName :: String
    , _ascription  :: Maybe Type
    , _expr        :: Expr
    }
  deriving Show

doParseModule :: String -> Either ParseError Module
doParseModule = parse module' ""

doParseFile :: FilePath -> IO (Either ParseError Module)
doParseFile path = doParseModule <$> readFile path

module' :: Parser Module
module' =
  Module <$ string "begin"
         <* whitespace
         <*> moduleIdentifier
         <* whitespace
         <*> many1 definition


{- syntax for definitions

<name> : type
<name> = binding

-- desugars to

<name> = <binding : type>

-}

definition :: Parser Definition
definition = do
  name <- identifier
  _ <- whitespace
  typedDefinition name <|> untypedDefinition name

  where
      typedDefinition name = do
        _ <- string ":"
        _ <- whitespace
        ty <- type'
        _ <- whitespace
        _ <- string name
        _ <- whitespace
        _ <- string "="
        _ <- whitespace
        v <- expr
        _ <- whitespace
        pure $ Definition name (Just ty) v
      untypedDefinition name = do
        _ <- string "="
        _ <- whitespace
        v <- expr
        _ <- whitespace
        pure $ Definition name Nothing v

{-| Grammar

lit = :numlit    #number
    | :stringlit #string
    | :arraylit  "[" expr ("," expr)* "]"

op = :operator #op

type = :bool   "Bool"
     | :num    "Num"
     | :string "String"
     | :unit   "Unit"
     | :cust   upvar
     | :paren  "(" type ")"
     | :arrow  type "->" type
     | :var    var

exp = :lit         lit
    | :application "(" exp+ ")"
    | :binop       exp op exp
    | :parentheses "(" exp ")"
    | :let         "let" var "=" exp "in" exp
    | :if          "if" exp "then" exp "else" exp
    | :lam         "\" var "." exp
    | :ascription  exp "::" type

-}

{-| AST specific for parsing.
    Gets desugared later.
-}
data Expr = NumLit Int
          | StringLit String
          | ListLit [Expr]
          | Asc Expr Type
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
expr = (try if' <?> "if")
   <|> (try lit <?> "lit")
   <|> (try let' <?> "let")
   <|> (Var <$> identifier <?> "identifier")
   <|> (appBinOp <?> "application / binop")
   <|> (lam <?> "lambda")
   <|> (parentheses <?> "parentheses")
   <|> (ascription <?> "ascription")

{- lit -}
lit :: Parser Expr
lit = numLit <|> stringLit <|> listLit

numLit :: Parser Expr
numLit = NumLit . read <$> many1 digit

listLit :: Parser Expr
listLit =
  ListLit
    <$  char '['
    <*> whitespaced expr `sepBy` char ','
    <*  char ']'

stringLit :: Parser Expr
stringLit = StringLit <$> parseString
  -- where block stolen from https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
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

{- let -}
let' :: Parser Expr
let' =
  Let <$  string "let"
      <*> whitespaced identifier
      <*  char '='
      <*> whitespaced expr
      <*  string "in"
      <*  whitespace
      <*> expr

{- binop / app -}
appBinOp :: Parser Expr
appBinOp = parens $ do
  lhs <- expr
  try (binop lhs) <|> app lhs

binop :: Expr -> Parser Expr
binop lhs = do
  let op = (Plus     <$ char '+')
       <|> (Minus    <$ char '-')
       <|> (Multiply <$ char '*')
       <|> (Cons     <$ string "::")
  op' <- whitespaced op
  rhs <- whitespaced expr
  pure $ BinOp op' lhs rhs

app :: Expr -> Parser Expr
app lhs =
  App . (lhs :|) <$> many (whitespaced expr)


{- parentheses -}
parentheses :: Parser Expr
parentheses = whitespaced $ char '(' *> whitespaced expr <* char ')'

{- if -}
if' :: Parser Expr
if' =
  If <$  string "if"
     <*> whitespaced expr
     <*  string "then"
     <*> whitespaced expr
     <*  string "else"
     <*> whitespaced expr

{- lam -}
lam :: Parser Expr
lam =
  Lam <$  char '\\'
      <*  whitespace
      <*> some1 identifier
      <*  whitespace
      <*  char '.'
      <*  whitespace
      <*> expr

{- types! -}
ascription :: Parser Expr
ascription = do
  char '<'
  expr <- expr
  whitespaced (string ":")
  ty <- type'
  char '>'
  pure $ Asc expr ty

type' :: Parser Type
type' = tyForall <|> tyArrow
  where
    tyParen = parens type'
    tyVar = TyVar <$> identifier
    tyForall = TyForall
      <$ string "forall"
      <*> whitespaced identifier
      <* string "."
      <* whitespace
      <*> type'
    tyBuiltin = whitespaced $
      (NumType <$ string "Num") <|>
      (BoolType <$ string "Bool") <|>
      (StringType <$ string "String") <|>
      (UnitType <$ string "Unit")
    tyArrow = do
      lhs <- tyParen <|> tyBuiltin <|> tyVar
      (try ((:~>) lhs <$ whitespaced (string "->") <*> type') <|> pure lhs)

{- util -}
whitespace :: Parser ()
whitespace = do
  _ <- many (oneOf [' ', '\t', '\n'])
  pure ()

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

whitespaced :: Parser a -> Parser a
whitespaced p = whitespace *> p <* whitespace

guarded :: (Alternative m, Monad m) => (a -> Bool) -> m a -> m a
guarded predicate a = do a' <- a
                         guard (predicate a')
                         pure a'

identifier :: Parser String
identifier =
  let sym =
        (:) <$> idFirst <*> many idRest
        where idFirst = oneOf (['_'] <> ['a'..'z'])
              idRest = oneOf (['_', '\''] <> ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'])
  in guarded (`notElem` ["if", "then", "else"]) sym

tyIdentifier :: Parser String
tyIdentifier =
  let sym =
        (:) <$> idFirst <*> many idRest
        where idFirst = oneOf (['_'] <> ['A'..'Z'])
              idRest = oneOf (['_', '\''] <> ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'])
  in guarded (`notElem` ["if", "then", "else"]) sym

moduleIdentifier :: Parser String
moduleIdentifier =
  let sym =
        (:) <$> idFirst <*> many idRest
        where idFirst = oneOf (['_'] <> ['A'..'Z'])
              idRest = oneOf (['.', '_', '\''] <> ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'])
  in guarded (`notElem` ["if", "then", "else"]) sym
