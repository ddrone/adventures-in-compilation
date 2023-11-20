{
module LVar.NewParser
  ( module LVar.NewParser
  , module LVar.NewParserDeps
  ) where

import LVar.Lexer (Token(..), TokenInfo(..))
import LVar.Operators (Binop(..), Unop(..))
import LVar.NewParserDeps
}

%name parse
%tokentype { (TokenInfo, Token) }
%error { parseError }
%monad { P } { thenP } { returnP }
%lexer { lexer } { (_, TokenEof) }

%token
  '(' { (_, TokenLit "(") }
  ')' { (_, TokenLit ")") }
  int { (_, TokenInt _) }
  ident { (_, TokenIdent _) }
  'True' { (_, TokenLit "True") }
  'False' { (_, TokenLit "False") }
  'input_int' { (_, TokenLit "input_int") }
  'if' { (_, TokenLit "if") }
  'else' { (_, TokenLit "else") }
  '{' { (_, TokenLit "{") }
  '}' { (_, TokenLit "}") }
  '[' { (_, TokenLit "[") }
  ']' { (_, TokenLit "]") }
  'print' { (_, TokenLit "print") }
  'while' { (_, TokenLit "while") }
  'tuple' { (_, TokenLit "tuple") }
  ';' { (_, TokenLit ";") }
  ',' { (_, TokenLit ",") }
  '=' { (_, TokenLit "=") }
  '+' { (_, TokenOp "+") }
  '-' { (_, TokenOp "-") }
  '<=' { (_, TokenOp "<=") }
  '<' { (_, TokenOp "<") }
  '>=' { (_, TokenOp ">=") }
  '>' { (_, TokenOp ">") }
  '==' { (_, TokenOp "==") }
  '!=' { (_, TokenOp "!=") }
  'and' { (_, TokenOp "and") }
  'or' { (_, TokenOp "or") }
  'not' { (_, TokenOp "not") }
  'is' { (_, TokenOp "is") }

%%

Module : Stmts { $1 }

Exp : Exp0 { $1 }

Exp0
  : Exp1 { $1 }
  | Exp1 'if' Exp1 'else' Exp1 { wrapNew $1 $5 (If $1 $3 $5) }

Exp1
  : Exp2 { $1 }
  | Exp1 'or' Exp2 { bin Or $1 $3 }

Exp2
  : Exp3 { $1 }
  | Exp2 'and' Exp3 { bin And $1 $3 }

Exp3
  : Exp4 { $1 }
  | Exp3 '==' Exp4 { bin Eq $1 $3 }
  | Exp3 '!=' Exp4 { bin Ne $1 $3 }
  | Exp3 'is' Exp4 { bin Is $1 $3 }

Exp4
  : Exp5 { $1 }
  | Exp4 '<=' Exp5 { bin Le $1 $3 }
  | Exp4 '<'  Exp5 { bin Lt $1 $3 }
  | Exp4 '>=' Exp5 { bin Ge $1 $3 }
  | Exp4 '>'  Exp5 { bin Gt $1 $3 }

Exp5
  : Exp6 { $1 }
  | Exp5 '+' Exp6 { bin Add $1 $3 }
  | Exp5 '-' Exp6 { bin Sub $1 $3 }

Exp6
  : Exp7 { $1 }
  | 'not' Exp6 { unary Not $1 $2 }
  | '-' Exp6 { unary Neg $1 $2 }

Exp7
  : Exp8 { $1 }
  | Exp7 '[' Exp ']' { wrapNew $1 $4 (Proj $1 $3) }

Exp8
  : '(' Exp ')' { wrap $1 $3 $2 }
  | int { lit $1 }
  | 'True' { constE $1 (Bool True) }
  | 'False' { constE $1 (Bool False) }
  | 'input_int' '(' ')' { (,) (combine (fst $1) (fst $3)) InputInt }
  | 'tuple' '(' Exps ')' { wrapNew $1 $4 (Tuple $3) }
  | ident { identE $1 }

Exps
  : {- empty -} { [] }
  | Exp { [$1] }
  | Exp ',' Exps { $1 : $3 }

Stmt
  : 'print' Exp { prefix Print $1 $2 }
  | 'if' Exp Block { wrapNew $1 $3 (IfS $2 $3 Nothing) }
  | 'if' Exp Block 'else' Block { wrapNew $1 $5 (IfS $2 $3 (Just $5)) }
  | 'while' Exp Block { wrapNew $1 $3 (While $2 $3) }
  | ident '=' Exp { wrapNew $1 $3 (Assign (identS $1) $3) }
  | Exp { (fst $1, Calc $1) }

Block
  : '{' '}' { (,) (combine (fst $1) (fst $2)) [] }
  | '{' Stmts '}' { wrap $1 $3 $2 }

Stmts
  : Stmt { (fst $1, [$1]) }
  | Stmt ';' Stmts { (,) (combine (fst $1) (fst $3)) ($1 : snd $3) }
