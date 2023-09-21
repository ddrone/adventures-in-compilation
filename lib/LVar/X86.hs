module LVar.X86 where

import Data.Char (toLower)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Void (Void, absurd)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Maybe (fromJust)

data ByteReg
  = Ah
  | Al
  | Bh
  | Bl
  | Ch
  | Cl
  | Dh
  | Dl
  deriving (Eq, Ord, Show)

data Cmp
  = E -- Equal
  | Ne -- Not equal
  | L -- Less than
  | Le -- Less than or equal
  | G -- Greater than
  | Ge -- Greater than or equal
  deriving (Eq, Ord, Show)

printCmp :: Cmp -> Text
printCmp c = Text.pack (map toLower (show c))

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
  deriving (Eq, Ord, Show)

printByteRegister :: ByteReg -> Text
printByteRegister r = Text.pack ('%' : map toLower (show r))

printRegister :: Reg -> Text
printRegister r = Text.pack ('%' : map toLower (show r))

data Arg n
  = Immediate Int64
  | Reg Reg
  | ByteReg ByteReg
  | Deref Reg Int64
  | Name n
  deriving (Eq, Ord, Show)

printArg :: (n -> Text) -> Arg n -> Text
printArg pn = \case
  Immediate c -> Text.pack ('$' : show c)
  Reg r -> printRegister r
  ByteReg r -> printByteRegister r
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
  | Callq Text Int -- arity of the called function - needed to compute live registers
  | Retq
  | Jump Text
  | Xorq (Arg n) (Arg n)
  | Cmpq (Arg n) (Arg n)
  | Set Cmp (Arg n)
  | Movzbq (Arg n) (Arg n)
  | JumpIf Cmp Text
  deriving (Show)

rawPrintInstr :: (n -> Text) -> GenInstr n -> Text
rawPrintInstr pn =
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
    Xorq a1 a2 -> binary "xorq" a1 a2
    Cmpq a1 a2 -> binary "cmpq" a1 a2
    Movzbq a1 a2 -> binary "movzbq" a1 a2
    Set c a -> unary ("set" <> printCmp c) a
    JumpIf c label -> Text.unwords ["j" <> printCmp c, label]
    Negq a -> unary "negq" a
    Pushq a -> unary "pushq" a
    Popq a -> unary "popq" a
    Callq label _ -> Text.unwords ["callq", label]
    Retq -> "retq"
    Jump label -> Text.unwords ["jmp", label]

printInstr :: GenInstr Void -> Text
printInstr instr = "    " <> rawPrintInstr absurd instr

printBlock :: Text -> Block Void -> [Text]
printBlock label instrs = (label <> ":") : map printInstr instrs

printProgram :: [GenInstr Void] -> [GenInstr Void] -> Program Void -> Text
printProgram prefix suffix program =
  let startBlock = fromJust (Map.lookup (progStartBlock program) (progBlocks program))
      otherBlocks = Map.toList (Map.delete (progStartBlock program) (progBlocks program))
      body = map printInstr startBlock ++ concatMap (uncurry printBlock) otherBlocks
      wholeBody = map printInstr prefix ++ body ++ ("conclusion:" : map printInstr suffix)
  in Text.unlines
    ( "    .globl main"
    : "main:"
    : wholeBody
    )

traverseInstr :: Applicative a => (Arg n -> a (Arg m)) -> GenInstr n -> a (GenInstr m)
traverseInstr f = \case
  Addq a1 a2 -> Addq <$> f a1 <*> f a2
  Subq a1 a2 -> Subq <$> f a1 <*> f a2
  Movq a1 a2 -> Movq <$> f a1 <*> f a2
  Negq a -> Negq <$> f a
  Pushq a -> Pushq <$> f a
  Popq a -> Popq <$> f a
  Callq t c -> pure (Callq t c)
  Retq -> pure Retq
  Jump t -> pure (Jump t)
  Xorq a1 a2 -> Xorq <$> f a1 <*> f a2
  Cmpq a1 a2 -> Cmpq <$> f a1 <*> f a2
  Movzbq a1 a2 -> Movzbq <$> f a1 <*> f a2
  Set c a -> Set c <$> f a
  JumpIf c label -> pure (JumpIf c label)

type Block n = [GenInstr n]

data Program n = Program
  { progStartBlock :: Text
  , progBlocks :: Map Text (Block n)
  }
  deriving (Show)

mapProgram :: (GenInstr n -> GenInstr m) -> Program n -> Program m
mapProgram f = mapProgramBlocks (map f)

mapProgramBlocks :: (Block n -> Block m) -> Program n -> Program m
mapProgramBlocks f (Program start blocks) = Program start (Map.map f blocks)

mapProgramM :: Monad f => (GenInstr n -> f (GenInstr m)) -> Program n -> f (Program m)
mapProgramM f (Program start blocks) = Program start <$> (mapM (mapM f) blocks)
