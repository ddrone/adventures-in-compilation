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
  "(" { action }
  ")" { action }
  "+" { action }
  "*" { action }
  $digit+ { action }

{
action (AlexPn offset row col) s = (offset, row, col, s)
}
