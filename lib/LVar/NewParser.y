{
module LVar.NewParser where

import LVar.Lexer (Token(..), TokenInfo(..))
}

%name parse
%tokentype { (TokenInfo, Token) }
%error { parseError }

%token
  '(' { (_, TokenLit "(") }
  ')' { (_, TokenLit ")") }
  '+' { (_, TokenLit "+") }
  '-' { (_, TokenLit "-") }
  int { (_, TokenInt _) }
  ident { (_, TokenIdent _) }
  'True' { (_, TokenLit "True") }
  'False' { (_, TokenLit "False") }
  'input_int' { (_, TokenLit "input_int") }
  'if' { (_, TokenLit "if") }
  'else' { (_, TokenLit "else") }
  '{' { (_, TokenLit "{") }
  '}' { (_, TokenLit "}") }
  'print' { (_, TokenLit "print") }
  'while' { (_, TokenLit "while") }
  '=' { (_, TokenLit "=") }


%%

Exp
  : Term { $1 }
  | Exp '+' Term { bin Add $1 $3 }
  | Exp '-' Term { bin Sub $1 $3 }

Term
  : int { lit $1 }
  | '(' Exp ')' { wrap $1 $3 $2 }

{
data Exp
  = Lit Integer
  | Add E E
  | Sub E E
  deriving (Show)

-- This is the actual return type of parser
type E = (TokenInfo, Exp)

lit (pos, TokenInt n) = (pos, Lit n)

-- Get a source span starting with the first argument and
-- ending with the second.
combine t1 t2 = t1 { tokEnd = tokEnd t2 }

-- Apply a binary tree constructor combining the source spans
bin f w1@(i1, _) w2@(i2, _) = (combine i1 i2, f w1 w2)

-- Wrap an existing tree into other tokens (for things like
-- brackets).
wrap (beg, _) (end, _) (_, t) = (combine beg end, t)

parseError _ = error "parse error!"
}
