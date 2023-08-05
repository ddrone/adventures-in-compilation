{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Text as Text

import AST

type Parser = Parsec Void Text

program :: Parser Program
program = many function

function :: Parser Function
function =
  Function <$>
    (symbol "function" *> ident) <*>
    (brackets (sepBy ident (symbol ","))) <*>
    block

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

number :: Parser LangInt
number = Lexer.decimal <* space

ident :: Parser Ident
ident = do
  first <- letterChar
  rest <- many alphaNumChar
  space
  pure (Text.pack (first : rest))

brackets = between (symbol "(") (symbol ")")
curlyBraces = between (symbol "{") (symbol "}")

expr :: Parser Exp
expr =
  (Lit <$> number) <|>
  do
    head <- ident
    (Call head <$> brackets (sepBy expr (symbol ","))) <|> pure (Var head)

stmt :: Parser Stmt
stmt = choice
  [ Return <$> (symbol "return" *> expr)
  , do
      var <- ident
      symbol "="
      e <- expr
      pure (Assign var e)
  ]

block :: Parser Block
block = curlyBraces (many (stmt <* symbol ";"))
