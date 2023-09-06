module LVar.ASTMon where

import Data.Int (Int64)
import Data.Text (Text)

import LVar.AST (Binop, Unop, GenModule)

data Name
  = Source Text
  | Gen Int
  deriving (Show)

data Atom
  = Const Int64
  | Name Name
  deriving (Show)

data Expr
  = Atom Atom
  | Bin Binop Atom Atom
  | Unary Unop Atom
  | InputInt
  deriving (Show)

data Stmt
  = Print Atom
  | Calc Expr
  | Assign Name Expr
  deriving (Show)

type Module = GenModule Stmt
