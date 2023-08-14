module Parser where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Text as Text

import AST

type Parser = Parsec Void Text

program :: Parser Program
program = spaceConsumer *> many function

function :: Parser Function
function =
  Function <$>
    (symbol "function" *> ident) <*>
    (brackets (sepBy ident (symbol ","))) <*>
    block

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 (Lexer.skipLineComment "#") (Lexer.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

number :: Parser LangInt
number = lexeme Lexer.decimal

ident :: Parser Ident
ident = lexeme $ do
  first <- letterChar
  rest <- many alphaNumChar
  pure (Text.pack (first : rest))

brackets = between (symbol "(") (symbol ")")
curlyBraces = between (symbol "{") (symbol "}")

term :: Parser Exp
term =
  (brackets expr) <|>
  (Lit <$> number) <|>
  do
    head <- ident
    (Call head <$> brackets (sepBy expr (symbol ","))) <|> pure (Var head)

expr = makeExprParser term table

elseBlock :: Parser Block
elseBlock = choice
  [ symbol "else" >> block
  , pure []
  ]

stmt :: Parser Stmt
stmt = choice
  [ Return <$> (symbol "return" *> expr <* symbol ";")
  , do
      symbol "if"
      cond <- brackets expr
      cons <- block
      alt <- elseBlock
      pure (If cond cons alt)
  , do
      var <- ident
      symbol "="
      e <- expr
      symbol ";"
      pure (Assign var e)
  ]

block :: Parser Block
block = curlyBraces (many stmt)

table =
  [ map unary [ Not, Neg ]
  , map binary [ Mul, Div, Mod ]
  , map binary [ Add, Sub ]
  , map binary [ Le, Lt, Ge, Gt ]
  , [ binary Equal ]
  , [ binary And ]
  , [ binary Or ]
  ]

binary op = InfixL (Bin op <$ symbol (binopRepr op))
unary op = Prefix (Unary op <$ symbol (unopRepr op))
