module AST where

import Data.Int (Int64)
import Data.Text (Text)

type LangInt = Int64
type Ident = Text

data Unop
  = Not
  | Neg
  deriving (Show)

data Binop
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lt
  | Le
  | Gt
  | Ge
  | Equal
  | And
  | Or
  deriving (Show)

binopRepr :: Binop -> Text
binopRepr op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Mod -> "%"
  Lt -> "<"
  Le -> "<="
  Gt -> ">"
  Ge -> ">="
  Equal -> "=="
  And -> "&&"
  Or -> "||"

unopRepr :: Unop -> Text
unopRepr op = case op of
  Not -> "!"
  Neg -> "-"

data Exp
  = Var Ident
  | Lit LangInt
  | Call Ident [Exp]
  | Bin Binop Exp Exp
  | Unary Unop Exp
  deriving (Show)

data Stmt
  = Assign Ident Exp
  | Return Exp
  | If Exp Block Block
  deriving (Show)

data Function = Function
  { fnName :: Ident
  , fnArgs :: [Ident]
  , fnBody :: Block
  }
  deriving (Show)

type Block = [Stmt]

type Program = [Function]
