module LVar.ExplicateControl where

import Prelude hiding (tail)

import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified LVar.AST as AST
import qualified LVar.ASTC as ASTC
import qualified LVar.ASTMon as ASTMon
import LVar.ASTMon (Name)
import Control.Monad (when)
import LVar.AST (isComparisonOp)

data ECState = ECState
  { ecsNextBlock :: Int
  , ecsBlocks :: IntMap ASTC.Block
  , ecsNextVar :: Int
  }

type EC a = State ECState a

freshBlockId :: EC Int
freshBlockId = do
  result <- gets ecsNextBlock
  modify $ \s -> s { ecsNextBlock = result + 1 }
  pure result

freshBlock :: Cont -> EC Int
freshBlock cont = do
  result <- freshBlockId
  modify $ \s -> s { ecsBlocks = IntMap.insert result cont (ecsBlocks s) }
  pure result

fresh :: EC ASTMon.Name
fresh = do
  next <- gets ecsNextVar
  modify $ \s -> s { ecsNextVar = next + 1 }
  pure (ASTMon.Gen next)

type Cont = ASTC.Block

goto :: Int -> Cont
goto label = ASTC.Block [] (ASTC.Goto label)

tail :: ASTC.Tail -> Cont
tail = ASTC.Block []

consStmt :: ASTC.Stmt -> Cont -> Cont
consStmt hd (ASTC.Block tl tail) = ASTC.Block (hd : tl) tail

explicateEffect :: ASTMon.Expr -> Cont -> EC Cont
explicateEffect expr cont = case expr of
  ASTMon.If cond cons alt -> do
    after <- freshBlockId
    consCont <- explicateEffect cons (goto after)
    altCont <- explicateEffect alt (goto after)
    explicatePred cond consCont altCont
  ASTMon.Atom _ -> pure cont
  ASTMon.Unary _ _ -> pure cont
  ASTMon.Bin _ _ _ -> pure cont -- parameters of binary expression are atoms, no effects here
  ASTMon.Begin ss e ->
    explicateEffect e =<< explicateBlock ss cont
  ASTMon.InputInt -> pure (consStmt (ASTC.Calc ASTC.InputInt) cont)

createBlock :: Cont -> EC Int
createBlock cont@(ASTC.Block stmts tail) = case (stmts, tail) of
  ([], ASTC.Goto l) -> pure l
  _ -> freshBlock cont

explicateAssign :: ASTMon.Expr -> Name -> Cont -> EC Cont
explicateAssign source target cont = case source of
  ASTMon.InputInt -> expr ASTC.InputInt
  ASTMon.Bin op a1 a2 -> expr (ASTC.Bin op a1 a2)
  ASTMon.Unary op a -> expr (ASTC.Unary op a)
  ASTMon.Atom a -> expr (ASTC.Atom a)
  ASTMon.Begin ss e -> explicateBlock ss =<< explicateAssign e target cont
  ASTMon.If cond cons alt -> do
    cont1 <- goto <$> createBlock cont
    consCont <- explicateAssign cons target cont1
    altCont <- explicateAssign alt target cont1
    explicatePred cond consCont altCont
  where
    expr e = stmt (ASTC.Assign target e)
    stmt s = pure (consStmt s cont)

explicatePred :: ASTMon.Cmp -> Cont -> Cont -> EC Cont
explicatePred expr consCont altCont = case expr of
  ASTMon.CmpAtom a -> do
    lCons <- createBlock consCont
    lAlt <- createBlock altCont
    let cmp = ASTC.AtomC a
    pure (tail (ASTC.CondJump cmp lCons lAlt))
  ASTMon.CmpLit True -> pure consCont
  ASTMon.CmpLit False -> pure altCont
  ASTMon.CmpOp op a1 a2 -> do
    lCons <- createBlock consCont
    lAlt <- createBlock altCont
    let cmp = ASTC.CmpC op a1 a2
    pure (tail (ASTC.CondJump cmp lCons lAlt))

explicateBlock :: ASTMon.Block -> Cont -> EC Cont
explicateBlock stmts cont = case stmts of
  [] -> pure cont
  hd : tl -> do
    cont1 <- explicateBlock tl cont
    explicateStatement hd cont1

explicateStatement :: ASTMon.Stmt -> Cont -> EC Cont
explicateStatement stmt cont = case stmt of
  ASTMon.Print atom -> pure (ASTC.Print atom `consStmt` cont)
  ASTMon.Calc e -> explicateEffect e cont
  ASTMon.IfS cond cons alt -> do
    contLabel <- createBlock cont
    consCont <- explicateBlock cons (goto contLabel)
    altCont <- explicateBlock alt (goto contLabel)
    explicatePred cond consCont altCont
  ASTMon.Assign n e -> explicateAssign e n cont

explicateControl :: ASTMon.Module -> Int -> ASTC.Module
explicateControl (AST.Module stmts) startVar =
  -- Starting to number blocks from 1 to leave 0 as ID for starting block
  let initState = ECState 1 IntMap.empty startVar
      ret = ASTC.Return (ASTC.Atom (ASTMon.Const 0))
      (startBlock, endState) = runState (explicateBlock stmts (tail ret)) initState
  in ASTC.Module startBlock (ecsBlocks endState)
