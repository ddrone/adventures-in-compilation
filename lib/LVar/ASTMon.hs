module LVar.ASTMon where

import Data.Int (Int64)
import Data.Text (Text)

import LVar.AST (Binop, Unop, GenStmt, GenModule)

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

type Stmt = GenStmt Name Expr

type Module = GenModule Stmt
