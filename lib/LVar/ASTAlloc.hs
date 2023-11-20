module LVar.ASTAlloc where
import Data.Int (Int64)
import Data.Text (Text)
import LVar.AST (Binop, Unop)
import LVar.Typechecker (Type)

data Name
  = Source Text
  | Gen Int
  deriving (Show)

data Expr ann
  = Const Int64
  | Bool Bool
  | Name Name
  | Bin Binop (E ann) (E ann)
  | If (E ann) (E ann) (E ann)
  | Unary Unop (E ann)
  | InputInt
  | Proj (E ann) (ann, Int)
  | Collect Int
  | Allocate Int Type
  | GlobalValue Text
  deriving (Show)

type E ann = (ann, Expr ann)
