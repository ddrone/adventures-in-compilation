module AST where

import Data.Int (Int64)
import Data.Text (Text)

type LangInt = Int64
type Ident = Text

data Binop
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  deriving (Show)

binopRepr :: Binop -> Text
binopRepr op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Mod -> "%"

data Exp
  = Var Ident
  | Lit LangInt
  | Call Ident [Exp]
  | Bin Binop Exp Exp
  deriving (Show)

data Stmt
  = Assign Ident Exp
  | Return Exp
  deriving (Show)

data Function = Function
  { fnName :: Ident
  , fnArgs :: [Ident]
  , fnBody :: Block
  }
  deriving (Show)

type Block = [Stmt]

type Program = [Function]
