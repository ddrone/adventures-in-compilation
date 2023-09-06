module LVar.Compiler where

import Control.Monad.State

import LVar.X86 (GenInstr(..), Reg(..))
import qualified LVar.AST as AST
import qualified LVar.ASTMon as ASTMon
import qualified LVar.X86 as X86

type RCO a = State Int a

fresh :: RCO ASTMon.Name
fresh = do
  next <- get
  modify (+1)
  pure (ASTMon.Gen next)

rcoAtom :: AST.Expr -> RCO (ASTMon.Atom, [ASTMon.Stmt])
rcoAtom e = case e of
  AST.Const n -> pure (ASTMon.Const n, [])
  AST.Name v -> pure (ASTMon.Name (ASTMon.Source v), [])
  _ -> do
    (le, ls) <- rcoExpr e
    name <- fresh
    pure (ASTMon.Name name, ls ++ [ASTMon.Assign name le])

rcoExpr :: AST.Expr -> RCO (ASTMon.Expr, [ASTMon.Stmt])
rcoExpr e = case e of
  AST.Bin op l r -> do
    (la, ls) <- rcoAtom l
    (ra, rs) <- rcoAtom r
    pure (ASTMon.Bin op la ra, ls ++ rs)
  AST.Unary op inner -> do
    (ia, is) <- rcoAtom inner
    pure (ASTMon.Unary op ia, is)
  AST.InputInt ->
    pure (ASTMon.InputInt, [])
  _ -> do
    (ea, es) <- rcoAtom e
    pure (ASTMon.Atom ea, es)

rcoStmt :: AST.Stmt -> RCO [ASTMon.Stmt]
rcoStmt = \case
  AST.Print e -> wrapAtom ASTMon.Print e
  AST.Calc e -> wrap ASTMon.Calc e
  AST.Assign n e -> wrap (ASTMon.Assign (ASTMon.Source n)) e
  where
    wrapAtom f e = do
      (ea, es) <- rcoAtom e
      pure (es ++ [f ea])
    wrap f e = do
      (ea, es) <- rcoExpr e
      pure (es ++ [f ea])

rcoModule :: AST.Module -> ASTMon.Module
rcoModule (AST.Module stmts) = flip evalState 0 $ do
  newStmts <- concat <$> mapM rcoStmt stmts
  pure (AST.Module newStmts)

type Instr = X86.GenInstr ASTMon.Name

type Arg = X86.Arg ASTMon.Name

atom :: ASTMon.Atom -> Arg
atom = \case
  ASTMon.Const c -> X86.Immediate c
  ASTMon.Name n -> X86.Name n

selectBinop :: AST.Binop -> Arg -> Arg -> Instr
selectBinop = \case
  AST.Add -> Addq
  AST.Sub -> Subq

selectUnop :: AST.Unop -> Arg -> Instr
selectUnop = \case
  AST.Neg -> Negq

selectExpr :: Arg -> ASTMon.Expr -> [Instr]
selectExpr dest = \case
  ASTMon.Atom a ->
    let src = atom a in
    if dest == src
      then []
      else [Movq src dest]
  ASTMon.Bin op a1 a2 ->
    let src1 = atom a1
        src2 = atom a2 in
    if
      | src1 == dest -> [ selectBinop op src2 dest ]
      | src2 == dest -> [ selectBinop op src1 dest ]
      | otherwise ->
        [ Movq src1 dest
        , selectBinop op src2 dest
        ]
  ASTMon.Unary op a ->
    let src = atom a in
    if dest == src
      then [ selectUnop op dest ]
      else
        [ Movq src dest
        , selectUnop op dest
        ]
  ASTMon.InputInt ->
    case dest of
      X86.Reg Rax -> [ Callq "read_int" ]
      _ ->
        [ Callq "read_int"
        , Movq (X86.Reg Rax) dest
        ]

selectStmt :: ASTMon.Stmt -> [Instr]
selectStmt = \case
  ASTMon.Print n ->
    [ Movq (atom n) (X86.Reg Rdi)
    , Callq "print_int"
    ]
  ASTMon.Calc e -> selectExpr (X86.Reg Rax) e
  ASTMon.Assign n e -> selectExpr (X86.Name n) e

selectInstructions :: ASTMon.Module -> [Instr]
selectInstructions (AST.Module stmts) = concatMap selectStmt stmts
