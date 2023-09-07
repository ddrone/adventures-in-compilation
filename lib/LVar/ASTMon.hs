module LVar.ASTMon where

import Control.Monad.State
import Data.Int (Int64)
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

import LVar.AST (Binop, Unop, GenModule, evalBinop, evalUnop)

data Name
  = Source Text
  | Gen Int
  deriving (Eq, Show, Ord)

data Atom
  = Const Int64
  | Name Name
  deriving (Show)

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

type Module = GenModule Stmt

type PE a = State (Map Name Int64) a

peAtom :: Atom -> PE Atom
peAtom = \case
  Const c -> pure (Const c)
  Name n -> do
    value <- gets (Map.lookup n)
    pure $ case value of
      Nothing -> Name n
      Just c -> Const c

peExpr :: Expr -> PE Expr
peExpr = \case
  Atom a -> Atom <$> peAtom a
  Bin op e1 e2 -> do
    p1 <- peAtom e1
    p2 <- peAtom e2
    pure $ case (p1, p2) of
      (Const c1, Const c2) -> Atom (Const (evalBinop op c1 c2))
      _ -> Bin op p1 p2
  Unary op e -> do
    p <- peAtom e
    pure $ case p of
      Const c -> Atom (Const (evalUnop op c))
      _ -> Unary op p
  InputInt -> pure InputInt

peStmt :: Stmt -> PE (Maybe Stmt)
peStmt = \case
  Print a -> Just . Print <$> peAtom a
  Calc e -> Just . Calc <$> peExpr e
  Assign n e -> do
    p <- peExpr e
    case p of
      Atom (Const c) -> do
        modify (Map.insert n c)
        pure Nothing
      _ -> do
        modify (Map.delete n)
        pure (Just (Assign n p))

partialEval :: [Stmt] -> [Stmt]
partialEval stmts = flip evalState Map.empty $
  catMaybes <$> mapM peStmt stmts
