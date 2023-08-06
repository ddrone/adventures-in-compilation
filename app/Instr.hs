module Instr where

import AST (Ident, LangInt)

data GenName
  = Src Ident Int
  | Gen Ident Int
  -- the important thing here is that identifier is not used for disambiguation,
  -- but only as a hint. Gen values can be compared only via the second parameter.
  deriving (Show)

data AssignSource
  = Var GenName
  | Lit LangInt
  | Call Ident [GenName]
  deriving (Show)

data Assign = Assign
  { asgnTarget :: GenName
  , asgnSource :: AssignSource
  }
  deriving (Show)

data BlockEnd
  = Ret GenName
  deriving (Show)

data Block = Block
  { blockName :: GenName -- names of blocks are in separate namespace from names of variables
  , blockAssigns :: [Assign]
  , blockEnd :: BlockEnd
  }
  deriving (Show)
