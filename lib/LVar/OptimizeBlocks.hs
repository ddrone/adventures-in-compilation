module LVar.OptimizeBlocks where

import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import LVar.ASTC
import Data.Maybe (fromJust)
import LVar.AST (Binop)
import LVar.ASTMon (Atom, Name)
import Data.Map (Map)
import Control.Monad.State (State)

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

optStmt :: Int -> Stmt -> BlockOpt Stmt
optStmt time stmt = undefined -- TODO: continue here
