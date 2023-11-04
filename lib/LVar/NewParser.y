{
module LVar.NewParser where

import LVar.Lexer (Token(..), TokenInfo(..))
import LVar.AST (Binop(..), Unop(..))
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
  'print' { (_, TokenLit "print") }
  'while' { (_, TokenLit "while") }
  ';' { (_, TokenLit ";") }
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

%%

Module : Stmts { $1 }

Exp : Exp0 { $1 }

Exp0
  : Exp1 { $1 }
  | Exp1 'if' Exp1 'else' Exp1 {% failP "Fixme" }

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
  : '(' Exp ')' { wrap $1 $3 $2 }
  | int { lit $1 }
  | 'True' { constE $1 (Bool True) }
  | 'False' { constE $1 (Bool False) }
  | 'input_int' '(' ')' { (,) (combine (fst $1) (fst $3)) InputInt }
  | ident { identE $1 }

Stmt
  : 'print' Exp { prefix Print $1 $2 }
  | 'if' Exp Block {% failP "handle if blocks" }
  | 'if' Exp Block 'else' Block {% failP "handle if-else blocks" }
  | 'while' Exp Block {% failP "handle while blocks" }
  | ident '=' Exp {% failP "handle assignments" }
  | Exp { (fst $1, Calc $1) }

Block
  : '{' '}' { (,) (combine (fst $1) (fst $2)) [] }
  | '{' Stmts '}' { wrap $1 $3 $2 }

Stmts
  : Stmt { (fst $1, [$1]) }
  | Stmt ';' Stmts { (,) (combine (fst $1) (fst $3)) ($1 : snd $3) }

{
data Expr
  = Const Int
  | Bool Bool
  | Name String
  | Bin Binop E E
  | If E E E
  | Unary Unop E
  | InputInt
  deriving (Show)

-- This is the actual return type of parser
type E = (TokenInfo, Expr)

data Stmt
  = Print E
  | Calc E
  | Assign String E
  | IfS E Block Block
  | While E Block
  deriving (Show)

type S = (TokenInfo, Stmt)
type Block = (TokenInfo, [S])

lit (pos, TokenInt n) = (pos, Const n)

-- Get a source span starting with the first argument and
-- ending with the second.
combine t1 t2 = t1 { tokEnd = tokEnd t2 }

-- Apply a binary tree constructor combining the source spans
bin op w1@(i1, _) w2@(i2, _) = (combine i1 i2, Bin op w1 w2)

prefix op (i1, _) w2@(i2, _) = (combine i1 i2, op w2)

unary op = prefix (Unary op)

constE (i1, _) v = (i1, v)

identE (i, TokenIdent v) = (i, Name v)

-- Wrap an existing tree into other tokens (for things like
-- brackets).
wrap (beg, _) (end, _) (_, t) = (combine beg end, t)

type Lexeme = (TokenInfo, Token)

data NextToken
  = NtEOF
  | NtToken TokenInfo
  deriving (Show)

type ParseError = (NextToken, String)

newtype P a = P { runP :: [Lexeme] -> Either ParseError (a, [Lexeme]) }

thenP (P a) f = P b
  where
    b input = case a input of
      Left pe -> Left pe
      Right (a, rest) -> runP (f a) rest

returnP a = P (\input -> Right (a, input))

failP :: String -> P a
failP err = P f
  where
    f input =
      let
        nt = case input of
          (info, _) : _ -> NtToken info
          [] -> NtEOF
      in Left (nt, err)

parseError :: Lexeme -> P a
parseError (info, _) = P (\input -> Left (NtToken info, "Happy parse error"))

lexer :: (Lexeme -> P a) -> P a
lexer = undefined
}
