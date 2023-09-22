module LVar.OptimizeBlocks where

import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import LVar.ASTC
import Data.Maybe (fromJust)
import LVar.AST (Binop, isComparisonOp)
import LVar.ASTMon (Atom, Name)
import Data.Map (Map)
import Control.Monad.State
import qualified Data.Map as Map
import LVar.ASTMon (Atom(..))

countBlockUses :: Module -> IntMap Int
countBlockUses (Module start blocks) = do
  let getBlock id = fromJust (IntMap.lookup id blocks)
  let go map visited queue = case queue of
        [] -> map
        hd : tl ->
          if IntSet.member hd visited
            then go map visited tl
            else do
              let edgesOut = tailOuts (blockTail (getBlock hd))
              let newMap = foldr (\k -> IntMap.insertWith (+) k 1) map edgesOut
              go map (IntSet.insert hd visited) (edgesOut ++ queue)
  let startVerts = tailOuts (blockTail start)
  let startMap = IntMap.fromList (map (flip (,) 1) startVerts)
  go startMap IntSet.empty startVerts

-- A block is inlinable if it either has only tail or is used exactly once.

-- The idea of what I want to implement is as follows:
-- 1. While optimizing a block, go through the sequences of instructions, using a counter.
-- 1.1. If a variable gets assigned to a result of comparison, save this in a map (with current counter)
-- 1.2. If a variable gets assigned to anything, save the last assignment time
-- 1.3. When having finished with a block:
-- 1.3.1. If tail is a goto, follow it immediately if it's inlinable.
-- 1.3.2. If tail is a conditional jump, see if it uses atom as a condition
--          If condition is both a comparison, and none of the values of comparison have
--          been overwritten in the mean time, inline the comparison into a goto.

data CmpValue = CmpValue
  { cvBinop :: Binop
  , cvArg1 :: Atom
  , cvArg2 :: Atom
  , cvTimeSaved :: Int
  }

data BlockOptState = BlockOptState
  { bosMap :: Map Name CmpValue
  , bosLastModified :: Map Name Int
  }

type BlockOpt a = State BlockOptState a

procStmt :: Int -> Stmt -> BlockOpt ()
procStmt time stmt = case stmt of
  Print _ -> pure ()
  Calc _ -> pure ()
  Assign n e -> do
    case e of
      Bin op a1 a2 | isComparisonOp op -> do
        let value = CmpValue op a1 a2 time
        modify $ \s -> s { bosMap = Map.insert n value (bosMap s) }
      _ -> pure ()
    modify $ \s -> s { bosLastModified = Map.insert n time (bosLastModified s) }

appendStmts :: [Stmt] -> Block -> Block
appendStmts ss (Block ss1 tail) = Block (ss ++ ss1) tail

isFreshEnough :: Int -> Atom -> BlockOpt Bool
isFreshEnough time atom = case atom of
  Name n -> do
    lastModified <- gets (Map.lookup n . bosLastModified)
    pure $ case lastModified of
      Nothing -> True
      Just v -> v < time
  Const _ -> pure True
  Bool _ -> pure True

optimizeBlock :: Module -> IntMap Int -> Block -> Block
optimizeBlock (Module start blocks) useCounts (Block ss tail) = do
  let canInline blockId =
        let singleUse = IntMap.lookup blockId useCounts == Just 1
            noStatements = null (blockStmts (fromJust (IntMap.lookup blockId blocks)))
        in singleUse || noStatements
  let procStmts start ss tail = do
        mapM_ (uncurry procStmt) (zip [start..] ss)
        case tail of
          Return _ -> pure (Block ss tail)
          Goto l ->
            if canInline l
              then
                let Block ss1 tail1 = fromJust (IntMap.lookup l blocks) in
                appendStmts ss <$> procStmts (start + length ss) ss1 tail1
              else
                pure (Block ss tail)
          CondJump cnd l1 l2 ->
            case cnd of
              AtomC (Name n) -> do
                v <- gets (Map.lookup n . bosMap)
                case v of
                  Nothing -> defaultOption
                  Just v1 -> do
                    ok1 <- isFreshEnough (cvTimeSaved v1) (cvArg1 v1)
                    ok2 <- isFreshEnough (cvTimeSaved v1) (cvArg2 v1)
                    if ok1 && ok2
                      then pure (Block ss (CondJump (CmpC (cvBinop v1) (cvArg1 v1) (cvArg2 v1)) l1 l2))
                      else defaultOption
              _ -> defaultOption
            where
              defaultOption = pure (Block ss tail)
  evalState (procStmts 0 ss tail) (BlockOptState Map.empty Map.empty)
