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

go pos s = (pos, TokenLit s)

number pos s = (pos, TokenInt (read s))
}
