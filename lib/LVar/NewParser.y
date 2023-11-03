{
module LVar.NewParser where

import LVar.Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  '(' { TokenLit "(" }
  ')' { TokenLit ")" }
  '+' { TokenLit "+" }
  '*' { TokenLit "*" }
  int { TokenInt $$ }

%%

Exp
  : Term { $1 }
  | Exp '+' Term { Add $1 $3 }

Term
  : Factor { $1 }
  | Term '*' Factor { Mul $1 $3 }

Factor
  : int { Lit $1 }
  | '(' Exp ')' { $2 }

{
data Exp
  = Lit Integer
  | Add Exp Exp
  | Mul Exp Exp
  deriving (Show)

parseError _ = error "parse error!"
}
