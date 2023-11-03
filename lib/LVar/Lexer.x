{
module LVar.Lexer where
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
  "*" { go }
  $digit+ { number }

{
data Token
  = TokenInt Integer
  | TokenLit String
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

number = action (TokenInt . read)
}
