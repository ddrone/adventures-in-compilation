module LVar.ASTC where
import Data.Int (Int64)
import LVar.ASTMon (Name, Atom)
import LVar.AST (Binop, Unop)
import Data.Text (Text)
import Data.IntMap (IntMap)

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

data Cond
  = AtomC Atom
  | CmpC Binop Atom Atom
  deriving (Show)

type Label = Int

data Tail
  = Return Expr
  | Goto Label
  | CondJump Cond Label Label
  deriving (Show)

data Block = Block
  { blockStmts :: [Stmt]
  , blockTail :: Tail
  }
  deriving (Show)

data Module = Module
  { moduleStart :: Block
  , moduleBlocks :: IntMap Block
  }
  deriving (Show)
