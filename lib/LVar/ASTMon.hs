module LVar.ASTMon where

import Control.Monad.State
import Data.Int (Int64)
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Text as Text

import LVar.AST (Binop, Unop, GenModule)
import LVar.PartialEval (evalBinop, evalUnop, Value)
import qualified LVar.PartialEval as PE

data Name
  = Source Text
  | Gen Int
  deriving (Eq, Show, Ord)

printName :: Name -> Text
printName = \case
  Source t -> t
  Gen n -> "$" <> Text.pack (show n)

data Atom
  = Const Int64
  | Bool Bool
  | Name Name
  deriving (Show)

printAtom :: Atom -> Text
printAtom = \case
  Const x -> Text.pack (show x)
  Bool b -> Text.pack (show b)
  Name n -> printName n

-- Not sure if I actually need this
data Cmp
  = CmpAtom Atom
  | CmpLit Bool
  | CmpOp Binop Atom Atom
  deriving (Show)

data Expr
  = Atom Atom
  | Bin Binop Atom Atom
  | Unary Unop Atom
  | If Cmp Expr Expr
  | Begin Block Expr
  | InputInt
  deriving (Show)

begin :: Expr -> Block -> Expr
begin e block = case block of
  [] -> e
  _ -> Begin block e

type Block = [Stmt]

data Stmt
  = Print Atom
  | Calc Expr
  | IfS Cmp Block Block
  | Assign Name Expr
  deriving (Show)

type Module = GenModule Stmt

type PE a = State (Map Name Value) a

peAtom :: Atom -> PE (Either Value Atom)
peAtom = \case
  Const c -> pure (Left (PE.Int64 c))
  Bool x -> pure (Left (PE.Bool x))
  Name n -> do
    value <- gets (Map.lookup n)
    pure $ case value of
      Nothing -> Right (Name n)
      Just c -> Left c

liftValue :: Value -> Atom
liftValue = \case
  PE.Int64 x -> Const x
  PE.Bool x -> Bool x

liftEitherValue :: Either Value Atom -> Atom
liftEitherValue = either liftValue id

peExpr :: Expr -> PE (Either Value Expr)
peExpr = \case
  Atom a -> (Atom <$>) <$> peAtom a
  Bin op e1 e2 -> do
    p1 <- peAtom e1
    p2 <- peAtom e2
    pure $ case (p1, p2) of
      (Left v1, Left v2) ->
        case evalBinop op v1 v2 of
          Just v -> Left v
          Nothing -> Right (Bin op (liftEitherValue p1) (liftEitherValue p2))
      _-> Right (Bin op (liftEitherValue p1) (liftEitherValue p2))
  Unary op e -> do
    p <- peAtom e
    pure $ case p of
      Left v ->
        case evalUnop op v of
          Just c -> Left c
          Nothing -> Right (Unary op (liftEitherValue p))
      _ -> Right (Unary op (liftEitherValue p))
  InputInt -> pure (Right InputInt)

peStmt :: Stmt -> PE (Maybe Stmt)
peStmt = \case
  Print a -> Just . Print . liftEitherValue <$> peAtom a
  Calc e -> Just . Calc . either (Atom . liftValue) id <$> peExpr e
  Assign n e -> do
    p <- peExpr e
    case p of
      Left c -> do
        modify (Map.insert n c)
        pure Nothing
      Right e -> do
        modify (Map.delete n)
        pure (Just (Assign n e))

partialEval :: [Stmt] -> [Stmt]
partialEval stmts = flip evalState Map.empty $
  catMaybes <$> mapM peStmt stmts
