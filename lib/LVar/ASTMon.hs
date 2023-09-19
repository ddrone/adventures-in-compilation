module LVar.ASTMon where

import Control.Monad.State
import Data.Int (Int64)
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Text as Text

import LVar.AST (Binop, Unop, GenModule, binopRepr, prependUnop)
import LVar.PartialEval (evalBinop, evalUnop, Value)
import qualified LVar.PartialEval as PE
import qualified LVar.AST as AST

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

printBinary :: Binop -> Atom -> Atom -> Text
printBinary op a1 a2 = Text.concat [printAtom a1, " ", binopRepr op, " ", printAtom a2]

printUnary :: Unop -> Atom -> Text
printUnary op a = prependUnop (printAtom a) op

printCmp :: Cmp -> Text
printCmp = \case
  CmpAtom atom -> printAtom atom
  CmpLit bool -> Text.pack (show bool)
  CmpOp op a1 a2 -> printBinary op a1 a2

data Expr
  = Atom Atom
  | Bin Binop Atom Atom
  | Unary Unop Atom
  | If Cmp Expr Expr
  | Begin Block Expr
  | InputInt
  deriving (Show)

bracket (t, flag) = case flag of
  True -> "(" <> t <> ")"
  False -> t

printExpr :: Int -> Expr -> (Text, Bool)
printExpr level expr =
  let indent = Text.pack (take (2 * level) (repeat ' '))
      content = case expr of
        Atom a -> (printAtom a, False)
        Bin op a1 a2 -> (printBinary op a1 a2, False)
        Unary op a -> (printUnary op a, False)
        If c e1 e2 ->
          let result = Text.concat
                [ "if "
                , printCmp c
                , " then "
                , bracket (printExpr level e1)
                , " else "
                , bracket (printExpr level e2)
                ]
          in (result, True)
        InputInt -> ("input_int()", False)
        Begin ss e ->
          let result = Text.intercalate "\n" $
                "begin {" :
                map (printStmt (level + 1)) ss ++
                [indent <> "} in " <> bracket (printExpr level e)]
          in (result, False)
  in case content of
       (t, flag) -> (indent <> t, flag)

printBlock :: Int -> Block -> Text
printBlock level ss = Text.intercalate "\n" (map (printStmt level) ss)

printStmt :: Int -> Stmt -> Text
printStmt level stmt =
  let indent = Text.pack (take (2 * level) (repeat ' '))
      content = case stmt of
        Print a -> Text.concat ["print(", printAtom a, ")"]
        Calc e -> fst (printExpr level e)
        IfS cmp cons alt -> Text.intercalate "\n"
          [ "if " <> printCmp cmp <> " {"
          , printBlock (level + 1) cons
          , indent <> "} else {"
          , printBlock (level + 1) alt
          , indent <> "}"
          ]
        Assign n e -> Text.concat [printName n, " = ", fst (printExpr level e)]
  in indent <> content

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

printModule :: Module -> Text
printModule (AST.Module ss) = printBlock 0 ss

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
