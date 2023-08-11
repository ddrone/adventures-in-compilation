module Graph.DominationFrontier where

import Data.Tree
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap

import Graph.Parser

childNodes :: Tree Int -> IntSet
childNodes (Node _ children) = foldMap allNodes children

allNodes :: Tree Int -> IntSet
allNodes = foldr IntSet.insert IntSet.empty

dominationFrontier :: ParsedGraph -> IntMap [Int]
dominationFrontier (ParsedGraph root _ g) =
  -- TODO: continue here
  undefined
