{
module LVar.Lexer where

-- Block comment, /* */
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum = [a-zA-Z0-9]

tokens :-
  $white+ ;
  "#".* ;
  "(" { go }
  ")" { go }
  "+" { go }
  "-" { go }
  $digit+ { number }
  "True" { go }
  "False" { go }
  "input_int" { go }
  "if" { go }
  "else" { go }
  "{" { go }
  "}" { go }
  "print" { go }
  "while" { go }
  "+" | "-" | "<=" | "<" | ">=" | ">" | "==" | "!=" | "and" | "or" | "not" { op }
  "=" { go }
  $alpha $alnum* { ident }

{
data Token
  = TokenInt Integer
  | TokenLit String
  | TokenIdent String
  | TokenOp String
  deriving (Show)

data TokenInfo = TokenInfo
  { tokOffset :: Int
  , tokRow :: Int
  , tokColumn :: Int
  , tokEnd :: Int
  }
  deriving (Show)

action f (AlexPn offset line row) s =
  (TokenInfo offset line row (offset + length s), f s)

go = action TokenLit

ident = action TokenIdent

number = action (TokenInt . read)

op = action TokenOp
}
