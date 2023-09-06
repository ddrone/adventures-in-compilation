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

traverseInstr :: Applicative a => (Arg n -> a (Arg m)) -> GenInstr n -> a (GenInstr m)
traverseInstr f = \case
  Addq a1 a2 -> Addq <$> f a1 <*> f a2
  Subq a1 a2 -> Subq <$> f a1 <*> f a2
  Movq a1 a2 -> Movq <$> f a1 <*> f a2
  Negq a -> Negq <$> f a
  Pushq a -> Pushq <$> f a
  Popq a -> Popq <$> f a
  Callq t -> pure (Callq t)
  Retq -> pure Retq
  Jump t -> pure (Jump t)

data Program n = Program
  { progInstrs :: [GenInstr n]
  }
  deriving (Show)
