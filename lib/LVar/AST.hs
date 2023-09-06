module LVar.AST where

import Data.Int (Int64)
import Data.Text (Text)

data Binop
  = Add
  | Sub
  deriving (Show)

binopRepr :: Binop -> Text
binopRepr = \case
  Add -> "+"
  Sub -> "-"

data Unop
  = Neg
  deriving (Show)

unopRepr :: Unop -> Text
unopRepr = \case
  Neg -> "-"

data Expr
  = Const Int64
  | Name Text
  | Bin Binop Expr Expr
  | Unary Unop Expr
  | InputInt
  deriving (Show)

data GenStmt n e
  = Print e
  | Calc e
  | Assign n e
  deriving (Show)

type Stmt = GenStmt Text Expr

data GenModule s = Module
  { modStmts :: [s]
  }
  deriving (Show)

type Module = GenModule Stmt
