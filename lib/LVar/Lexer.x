{
module LVar.Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum = [a-zA-Z0-9]

tokens :-
  $white+ ;
  "#".* ;
  $digit+ { id }