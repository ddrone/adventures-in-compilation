{
module LVar.NewParser where

import Data.Int (Int64)
import Data.Text (Text)

import LVar.Lexer (Token(..), TokenInfo(..))
import LVar.Operators (Binop(..), Unop(..))
import LVar.ParseTree
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

{
data Expr ann
  = Const Int64
  | Bool Bool
  | Name Text
  | Bin Binop (E ann) (E ann)
  | If (E ann) (E ann) (E ann)
  | Unary Unop (E ann)
  | InputInt
  | Tuple [E ann]
  | Proj (E ann) (E ann)
  deriving (Show)

instance Functor Expr where
  fmap f e = case e of
    Const c -> Const c
    Bool b -> Bool b
    Name t -> Name t
    Bin op e1 e2 -> Bin op (go e1) (go e2)
    If cond cons alt -> If (go cond) (go cons) (go alt)
    Unary op e -> Unary op (go e)
    InputInt -> InputInt
    Tuple es -> Tuple (map go es)
    Proj tup i -> Proj (go tup) (go i)
    where
      go (i, e) = (f i, fmap f e)

-- This is the actual return type of parser
type E ann = (ann, Expr ann)

instance ToParseTree (TokenInfo, Expr TokenInfo) where
  toParseTree (ti, e) =
    let
      go :: String -> [E TokenInfo] -> ParseTree
      go name children = ParseTree name ti (toParseForest children)
    in
    case e of
      Const _ -> go "Const" []
      Bool _ -> go "Bool" []
      Name _ -> go "Name" []
      Bin _ l r -> go "Bin" [l, r]
      If cond cons alt -> go "If" [cond, cons, alt]
      Unary _ e -> go "Unary" [e]
      InputInt -> go "InputInt" []
      Tuple es -> go "Tuple" es
      Proj tup i -> go "Projection" [tup, i]

data Stmt ann
  = Print (E ann)
  | Calc (E ann)
  | Assign (ann, Text) (E ann)
  | IfS (E ann) (Block ann) (Maybe (Block ann))
  | While (E ann) (Block ann)
  deriving (Show)

instance Functor Stmt where
  fmap f e = case e of
    Print e -> Print (goE e)
    Calc e -> Calc (goE e)
    Assign (a, name) e -> Assign (f a, name) (goE e)
    IfS cond cons mAlt -> IfS (goE cond) (goSS cons) (fmap goSS mAlt)
    While cond block -> While (goE cond) (goSS block)
    where
      goE (i, e) = (f i, fmap f e)
      go (i, s) = (f i, fmap f s)
      goSS (i, ss) = (f i, map go ss)

type S ann = (ann, Stmt ann)
type Block ann = (ann, [S ann])

instance ToParseTree (TokenInfo, Stmt TokenInfo) where
  toParseTree (ti, s) =
    let go name children = ParseTree name ti children in
    case s of
      Print e -> go "Print" [toParseTree e]
      Calc e -> go "Calc" [toParseTree e]
      Assign (nameInfo, name) e -> go "Assign" [ParseTree "Name" nameInfo [], toParseTree e]
      IfS cond cons mAlt ->
        case mAlt of
          Nothing -> go "IfS" [toParseTree cond, toParseTree cons]
          Just alt -> go "IfS" [toParseTree cond, toParseTree cons, toParseTree alt]
      While cond block -> go "While" [toParseTree cond, toParseTree block]

instance ToParseTree (TokenInfo, [S TokenInfo]) where
  toParseTree (info, stmts) = ParseTree "Block" info (toParseForest stmts)

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

identS (i, TokenIdent v) = (i, v)

-- Wrap an existing tree into other tokens (for things like
-- brackets).
wrap (beg, _) (end, _) (_, t) = (combine beg end, t)

wrapNew (beg, _) (end, _) t = (combine beg end, t)

type Lexeme = (TokenInfo, Token)

type ParseError = (TokenInfo, String)

newtype P a = P { runP :: [Lexeme] -> Either ParseError (a, [Lexeme]) }

thenP (P a) f = P b
  where
    b input = case a input of
      Left pe -> Left pe
      Right (a, rest) -> runP (f a) rest

returnP a = P (\input -> Right (a, input))

failWrap (i1, _) (i2, _) err = failP (combine i1 i2) err

failP :: TokenInfo -> String -> P a
failP info err = P (const (Left (info, err)))

parseError :: Lexeme -> P a
parseError (info, _) = P (\input -> Left (info, "Happy parse error"))

nextLexeme :: P Lexeme
nextLexeme = P f
  where
    f [] = error "Internal parse error: trying to access lexemes beyond EOF"
    f (first : rest) = Right (first, rest)

lexer :: (Lexeme -> P a) -> P a
lexer = thenP nextLexeme
}
