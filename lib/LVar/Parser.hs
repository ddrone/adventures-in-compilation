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
  (symbol "true" >> pure (Bool True)) <|>
  (symbol "false" >> pure (Bool False)) <|>
  (symbol "input_int" >> symbol "(" >> symbol ")" >> pure InputInt) <|>
  (Name <$> ident)

expr :: Parser Expr
expr = do
  pe <- preexpr
  choice
    [ do try (symbol "if")
         cond <- preexpr
         symbol "else"
         alt <- preexpr
         pure (If cond pe alt)
    , pure pe
    ]

preexpr = makeExprParser term table

block :: Parser Block
block = between (symbol "{") (symbol "}") (many stmt)

stmt :: Parser Stmt
stmt = choice
  [ Print <$> (symbol "print" *> brackets expr)
  , do
      symbol "if"
      e <- expr
      cons <- block
      alt <- choice
        [ symbol "else" >> block
        , pure []
        ]
      pure (IfS e cons alt)
  , do
      var <- ident
      symbol "="
      e <- expr
      pure (Assign var e)
  , Calc <$> expr
  ]

table =
  [ map unary [ Neg, Not ]
  , map binary [ Add, Sub ]
  , map binary [ Le, Lt, Ge, Gt ]
  , map binary [ Eq, Ne ]
  , [ binary And ]
  , [ binary Or ]
  ]

binary op = InfixL (Bin op <$ symbol (binopRepr op))
unary op = Prefix (Unary op <$ symbol (unopRepr op))
