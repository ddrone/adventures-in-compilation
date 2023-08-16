module CFG.Instr where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text

import AST (Ident, LangInt, Binop, Unop)

data GenName
  = Src Ident
  | Gen Ident Int
  -- the important thing here is that identifier is not used for disambiguation,
  -- but only as a hint. Gen values can be compared only via the second parameter.
  deriving (Show)

data BlockName = BlockName
  { bnFnName :: Ident
  , bnId :: Int
  }
  deriving (Show, Eq, Ord)

printBlockName :: BlockName -> Text
printBlockName (BlockName fn n) =
  Text.concat [fn, "$", Text.pack (show n)]

data AssignSource
  = Var GenName
  | Lit LangInt
  | Call Ident [GenName]
  | Bin Binop GenName GenName
  | Unary Unop GenName
  deriving (Show)

data Assign = Assign
  { asgnTarget :: GenName
  , asgnSource :: AssignSource
  }
  deriving (Show)

data BlockEnd
  = Ret GenName
  | Jump BlockName
  | CondJump GenName BlockName BlockName
  deriving (Show)

data Block = Block
  { blockName :: BlockName
  , blockAssigns :: [Assign]
  , blockEnd :: BlockEnd
  }
  deriving (Show)

-- Potentially looks quite similar to AST.Function, but I'm not sure whether it
-- makes sense to generalize the data structure for it right now.
data Function = Function
  { fnName :: Ident
  , fnArgs :: [Ident]
  , fnBody :: [Block]
  , fnStartLabel :: BlockName
  }
  deriving (Show)
