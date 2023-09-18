module LVar.AST where

import Data.Int (Int64)
import Data.Text (Text)

data Binop
  = Add
  | Sub
  | Le
  | Lt
  | Ge
  | Gt
  | Eq
  | Ne
  | And
  | Or
  deriving (Show)

evalBinop :: Binop -> Int64 -> Int64 -> Int64
evalBinop = \case
  Add -> (+)
  Sub -> (-)

binopRepr :: Binop -> Text
binopRepr = \case
  Add -> "+"
  Sub -> "-"
  Le -> "<="
  Lt -> "<"
  Ge -> ">="
  Gt -> ">"
  Eq -> "=="
  Ne -> "!="
  And -> "and"
  Or -> "or"

data Unop
  = Neg
  | Not
  deriving (Show)

unopRepr :: Unop -> Text
unopRepr = \case
  Neg -> "-"
  Not -> "!"

evalUnop :: Unop -> Int64 -> Int64
evalUnop = \case
  Neg -> (0 -)

data Expr
  = Const Int64
  | Bool Bool
  | Name Text
  | Bin Binop Expr Expr
  | If Expr Expr Expr
  | Unary Unop Expr
  | InputInt
  deriving (Show)

data Stmt
  = Print Expr
  | Calc Expr
  | Assign Text Expr
  deriving (Show)

data GenModule s = Module
  { modStmts :: [s]
  }
  deriving (Show)

type Module = GenModule Stmt
