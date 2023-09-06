module LVar.Expr where

import Data.Int (Int64)

data Binop
  = Add
  | Sub
  deriving (Show)

data Unop
  = Neg
  deriving (Show)

data Expr
  = Const Int64
  | Bin Expr Binop Expr
  | Unary Unop Expr
  | InputInt
  deriving (Show)

data Stmt
  = Print Expr
  | Calc Expr
  deriving (Show)
