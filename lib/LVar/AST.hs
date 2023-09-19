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
  Not -> "not"

data Expr
  = Const Int64
  | Bool Bool
  | Name Text
  | Bin Binop Expr Expr
  | If Expr Expr Expr
  | Unary Unop Expr
  | InputInt
  deriving (Show)

exprPlate :: (Expr -> Expr) -> Expr -> Expr
exprPlate c e = case e of
  -- Apply the function to constructors that actually have children
  Bin op e1 e2 -> Bin op (c e1) (c e2)
  If cond cons alt -> If (c cond) (c cons) (c alt)
  Unary op e -> Unary op (c e)
  -- Don't do anything to the constructors that do not have children
  Const _ -> e
  Bool _ -> e
  Name _ -> e
  InputInt -> e

exprTopdown :: (Expr -> Expr) -> Expr -> Expr
exprTopdown transform = transform . exprPlate (exprTopdown transform)

type Block = [Stmt]

data Stmt
  = Print Expr
  | Calc Expr
  | Assign Text Expr
  | IfS Expr Block Block
  deriving (Show)

mapExpr :: (Expr -> Expr) -> Stmt -> Stmt
mapExpr f = \case
  Print e -> Print (f e)
  Calc e -> Calc (f e)
  Assign n e -> Assign n (f e)
  IfS cond cons alt -> IfS (f cond) (map (mapExpr f) cons) (map (mapExpr f) alt)

data GenModule s = Module
  { modStmts :: [s]
  }
  deriving (Show)

type Module = GenModule Stmt

mapModule :: (Expr -> Expr) -> Module -> Module
mapModule f (Module stmts) = Module (map (mapExpr f) stmts)
