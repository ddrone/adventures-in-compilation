{
module LVar.NewParser where

import LVar.Lexer (Token(..), TokenInfo)
}

%name parse
%tokentype { (TokenInfo, Token) }
%error { parseError }

%token
  '(' { (_, TokenLit "(") }
  ')' { (_, TokenLit ")") }
  '+' { (_, TokenLit "+") }
  '*' { (_, TokenLit "*") }
  int { (_, TokenInt _) }

%%

Exp
  : Term { $1 }
  | Exp '+' Term { Add $1 $3 }

Term
  : Factor { $1 }
  | Term '*' Factor { Mul $1 $3 }

Factor
  : int { lit $1 }
  | '(' Exp ')' { $2 }

{
data Exp
  = Lit (TokenInfo, Integer)
  | Add Exp Exp
  | Mul Exp Exp
  deriving (Show)

lit (pos, TokenInt n) = Lit (pos, n)

parseError _ = error "parse error!"
}
