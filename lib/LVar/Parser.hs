module LVar.Parser where

import Control.Monad.Combinators.Expr
import Data.Int (Int64)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Text as Text

import LVar.AST

type Parser = Parsec Void Text

program :: Parser Module
program = Module <$> (spaceConsumer *> many stmt)

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 (Lexer.skipLineComment "#") (Lexer.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

number :: Parser Int64
number = lexeme Lexer.decimal

ident :: Parser Text
ident = lexeme $ do
  first <- letterChar
  rest <- many alphaNumChar
  pure (Text.pack (first : rest))

brackets = between (symbol "(") (symbol ")")

term :: Parser Expr
term =
  (brackets expr) <|>
  (Const <$> number) <|>
  (symbol "input_int" >> symbol "(" >> symbol ")" >> pure InputInt) <|>
  (Name <$> ident)

expr = makeExprParser term table

stmt :: Parser Stmt
stmt = choice
  [ Print <$> (symbol "print" *> brackets expr)
  , do
      var <- ident
      symbol "="
      e <- expr
      pure (Assign var e)
  , Calc <$> expr
  ]

table =
  [ map unary [ Neg ]
  , map binary [ Add, Sub ]
  ]

binary op = InfixL (Bin op <$ symbol (binopRepr op))
unary op = Prefix (Unary op <$ symbol (unopRepr op))
