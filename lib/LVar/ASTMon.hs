module LVar.ASTMon where

import Control.Monad.State
import Data.Int (Int64)
import Data.List (singleton)
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Text as Text

import LVar.AST (Binop, Unop, GenModule, binopRepr, prependUnop)
import LVar.PartialEval (evalBinop, evalUnop, Value)
import Utils (runLocal)
import qualified LVar.PartialEval as PE
import qualified LVar.AST as AST

data Name
  = Source Text
  | Gen Int
  deriving (Eq, Show, Ord)

printName :: Name -> Text
printName = \case
  Source t -> t
  Gen n -> "@" <> Text.pack (show n)

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
  | CmpOp Binop Atom Atom
  deriving (Show)

printBinary :: Binop -> Atom -> Atom -> Text
printBinary op a1 a2 = Text.concat [printAtom a1, " ", binopRepr op, " ", printAtom a2]

printUnary :: Unop -> Atom -> Text
printUnary op a = prependUnop (printAtom a) op

printCmp :: Cmp -> Text
printCmp = \case
  CmpAtom atom -> printAtom atom
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
  let indent = Text.pack (take (2 * level) (repeat ' ')) in
  case expr of
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
        While condDeps cond body -> Text.intercalate "\n"
          [ "while ("
          , printBlock (level + 1) condDeps
          , indent <> "  " <> printCmp cond
          , indent <> ") {"
          , printBlock (level + 1) body
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
  | While Block Cmp Block
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

toAtom :: Either Value Atom -> Atom
toAtom = either liftValue id

toExpr :: Either Value Expr -> Expr
toExpr = either (Atom . liftValue) id

toCmp :: Either Value Cmp -> Cmp
toCmp = either (CmpAtom . liftValue) id

peCmp :: Cmp -> PE (Either Value Cmp)
peCmp = \case
  CmpAtom a -> do
    v <- peAtom a
    case v of
      Left v -> pure (Left v)
      Right a -> pure (Right (CmpAtom a))
  CmpOp op e1 e2 -> do
    p1 <- peAtom e1
    p2 <- peAtom e2
    pure $ case (p1, p2) of
      (Left v1, Left v2) ->
        case evalBinop op v1 v2 of
          Just v -> Left v
          Nothing -> Right (CmpOp op (toAtom p1) (toAtom p2))
      _-> Right (CmpOp op (toAtom p1) (toAtom p2))

mergeMaps :: (Ord k, Eq v) => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = Map.filterWithKey (\k v -> Map.lookup k m2 == Just v) m1

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
          Nothing -> Right (Bin op (toAtom p1) (toAtom p2))
      _-> Right (Bin op (toAtom p1) (toAtom p2))
  Unary op e -> do
    p <- peAtom e
    pure $ case p of
      Left v ->
        case evalUnop op v of
          Just c -> Left c
          Nothing -> Right (Unary op (toAtom p))
      _ -> Right (Unary op (toAtom p))
  InputInt -> pure (Right InputInt)
  Begin ss e -> do
    ss1 <- peBlock ss
    v <- peExpr e
    case ss1 of
      [] -> pure v
      _ -> pure (Right (Begin ss1 (toExpr v)))
  If cond cons alt -> do
    condV <- peCmp cond
    case condV of
      Left (PE.Bool True) -> peExpr cons
      Left (PE.Bool False) -> peExpr alt
      _ -> do
        (consE, consM) <- runLocal (toExpr <$> peExpr cons)
        (altE, altM) <- runLocal (toExpr <$> peExpr alt)
        put (mergeMaps consM altM)
        pure (Right (If (toCmp condV) consE altE))

peBlock :: [Stmt] -> PE [Stmt]
peBlock ss = concat <$> mapM peStmt ss

clearBlock :: [Stmt] -> PE ()
clearBlock = mapM_ clearStmt

clearStmt :: Stmt -> PE ()
clearStmt = \case
  Print _ -> pure ()
  Calc _ -> pure ()
  Assign n _ -> modify (Map.delete n)
  IfS _ cons alt -> clearBlock cons >> clearBlock alt
  While condDeps _ body -> clearBlock condDeps >> clearBlock body

peStmt :: Stmt -> PE [Stmt]
peStmt stmt = case stmt of
  Print a -> singleton . Print . toAtom <$> peAtom a
  Calc e -> singleton . Calc . toExpr <$> peExpr e
  Assign n e -> do
    p <- peExpr e
    case p of
      Left c -> do
        modify (Map.insert n c)
        pure []
      Right e -> do
        modify (Map.delete n)
        pure [Assign n e]
  IfS cond cons alt -> do
    condV <- peCmp cond
    case condV of
      Left (PE.Bool True) -> peBlock cons
      Left (PE.Bool False) -> peBlock alt
      _ -> do
        (consSS, consM) <- runLocal (peBlock cons)
        (altSS, altM) <- runLocal (peBlock alt)
        put (mergeMaps consM altM)
        pure [IfS (toCmp condV) consSS altSS]
  While condDeps cond body -> do
    currState <- get
    let (result, nextState) = flip runState currState $ do
          peBlock condDeps
          peCmp cond
    case result of
      -- Condition evaluated to False right away, the loop is not going to be executed
      Left (PE.Bool False) -> do
        put nextState
        pure []
      -- In literally other case we need to traverse the block, remove all the variables
      -- that can be possibly set in either condition or the body
      _ -> do
        clearStmt stmt
        pure [stmt]

partialEval :: [Stmt] -> [Stmt]
partialEval stmts = evalState (peBlock stmts) Map.empty
