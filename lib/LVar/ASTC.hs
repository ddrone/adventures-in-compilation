module LVar.ASTC where

import Data.Int (Int64)
import Data.List (intercalate, intersperse)
import Data.Text (Text)
import Data.IntMap (IntMap)
import qualified Data.Text as Text
import qualified Data.IntMap as IntMap

import LVar.ASTMon (Name, Atom, printAtom, printName)
import LVar.AST (Binop, Unop, prependUnop, binopRepr)
import DirectedGraph (Graph)
import qualified DirectedGraph as Graph

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

printExpr :: Expr -> Text
printExpr = \case
  Atom a -> printAtom a
  Bin op a1 a2 -> Text.concat [printAtom a1, " ", binopRepr op, " ", printAtom a2]
  Unary op a -> prependUnop (printAtom a) op
  InputInt -> "input_int()"

printStmt :: Stmt -> Text
printStmt = \case
  Print a -> Text.concat ["  print(", printAtom a, ")"]
  Calc e -> "  " <> printExpr e
  Assign n e -> "  " <> printName n <> " = " <> printExpr e

printLabel :: Label -> Text
printLabel l = "block_" <> Text.pack (show l)

printCond :: Cond -> Text
printCond = \case
  AtomC a -> printAtom a
  CmpC op a1 a2 -> Text.concat [printAtom a1, " ", binopRepr op, " ", printAtom a2]

printTail :: Tail -> Text
printTail = \case
  Return e -> "  return " <> printExpr e
  Goto l -> "  goto " <> printLabel l
  CondJump c cons alt -> Text.concat
    [ "  if " <> printCond c <> "\n"
    , "    then jump " <> printLabel cons <> "\n"
    , "    else jump " <> printLabel alt
    ]

printBlock :: Block -> [Text]
printBlock (Block stmts tail) = map printStmt stmts ++ [printTail tail]

printModule :: Module -> Text
printModule mod@(Module start blocks) =
  let printLabeledBlock label block = (printLabel label <> ":") : printBlock block
      lines = concat (intersperse [Text.pack ""] (printBlock start : map (uncurry printLabeledBlock) (IntMap.toList blocks)))
      topSortLine = Text.concat ["topsort: ", Text.pack (show (Graph.topologicalSort (toGraph mod) 0))]
  in Text.unlines (lines ++ ["", topSortLine])

toGraph :: Module -> Graph Int
toGraph (Module start blocks) =
  let blockEdge id block = case blockTail block of
        Return _ -> []
        Goto l -> [(id, l)]
        CondJump _ l1 l2 -> [(id, l1), (id, l2)]
  in foldr (uncurry Graph.addEdge) Graph.empty (concatMap (uncurry blockEdge) ((0, start) : IntMap.toList blocks))
