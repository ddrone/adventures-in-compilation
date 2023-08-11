module Graph.DominationFrontier where

import Control.Arrow ((&&&))
import Data.Tree
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.STRef
import Debug.Trace (traceM, traceShowM)
import GHC.ST (runST)
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap

import Graph.Parser
import Graph.DominatorTree (dominatorTree, invertTree)
import Graph.Defs (edgesFrom)

childNodes :: Tree Int -> IntSet
childNodes (Node _ children) = foldMap allNodes children

allNodes :: Tree Int -> IntSet
allNodes = foldr IntSet.insert IntSet.empty

treeWithChildNodes :: Tree Int -> Tree (Int, IntSet)
treeWithChildNodes (Node root children) = do
  let mappedChildren = map treeWithChildNodes children
  let allChildren = foldMap (uncurry IntSet.insert) $ map rootLabel mappedChildren
  Node (root, allChildren) mappedChildren

dominationFrontier :: ParsedGraph -> IntMap IntSet
dominationFrontier pg@(ParsedGraph root _ g) = do
  let dt = flatten $ treeWithChildNodes $ invertTree root $ dominatorTree pg
  let computeFrontier root children = do
        let outgoingEdges = foldMap (IntSet.fromList . edgesFrom g) (root : IntSet.toList children)
        IntSet.difference outgoingEdges children
  IntMap.fromList $ map (fst &&& uncurry computeFrontier) dt
