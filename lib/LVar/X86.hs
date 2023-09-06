module LVar.X86 where
import Data.Int (Int64)
import Data.Text (Text)

data Reg
  = Rsp
  | Rbp
  | Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Eq, Show)

data Arg n
  = Immediate Int64
  | Reg Reg
  | Deref Reg Int64
  | Name n
  deriving (Eq, Show)

data GenInstr n
  = Addq (Arg n) (Arg n)
  | Subq (Arg n) (Arg n)
  | Movq (Arg n) (Arg n)
  | Negq (Arg n)
  | Pushq (Arg n)
  | Popq (Arg n)
  | Callq Text
  | Retq
  | Jump Text
  deriving (Show)

data Program n = Program
  { progInstrs :: [GenInstr n]
  }
  deriving (Show)
