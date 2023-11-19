module LVar.Operators where

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
  | Is
  deriving (Show, Eq, Ord)

data Unop
  = Neg
  | Not
  deriving (Show)
