module LVar.X86 where

import Data.Char (toLower)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Void (Void, absurd)
import qualified Data.Text as Text

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

printRegister :: Reg -> Text
printRegister r = Text.pack ('%' : map toLower (show r))

data Arg n
  = Immediate Int64
  | Reg Reg
  | Deref Reg Int64
  | Name n
  deriving (Eq, Show)

printArg :: (n -> Text) -> Arg n -> Text
printArg pn = \case
  Immediate c -> Text.pack ('$' : show c)
  Reg r -> printRegister r
  Deref r offset -> Text.concat
    [ Text.pack (show offset)
    , "("
    , printRegister r
    , ")"
    ]
  Name n -> pn n

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

printInstr :: (n -> Text) -> GenInstr n -> Text
printInstr pn =
  let pa = printArg pn
      binary instr a1 a2 = Text.concat
        [ instr
        , " "
        , pa a1
        , ", "
        , pa a2
        ]
      unary instr a = Text.concat
        [ instr
        , " "
        , pa a
        ]
  in
  \case
    Addq a1 a2 -> binary "addq" a1 a2
    Subq a1 a2 -> binary "subq" a1 a2
    Movq a1 a2 -> binary "movq" a1 a2
    Negq a -> unary "negq" a
    Pushq a -> unary "pushq" a
    Popq a -> unary "popq" a
    Callq label -> Text.unwords ["callq", label]
    Retq -> "retq"
    Jump label -> Text.unwords ["jump", label]

printProgram :: [GenInstr Void] -> Text
printProgram instrs = Text.unlines
  ( "    .globl main"
  : "main:"
  : map (Text.append "    " . printInstr absurd) instrs
  )

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
