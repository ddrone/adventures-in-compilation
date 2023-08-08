module Graph.Defs where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Utils (multimapInsert)
import Data.Maybe (fromMaybe)

data Graph = Graph
  { graphEdges :: IntMap [Int]
  , graphBackEdges :: IntMap [Int]
  }
  deriving (Show)

insertEdge :: (Int, Int) -> Graph -> Graph
insertEdge (from, to) g = Graph
  { graphEdges = multimapInsert from to (graphEdges g)
  , graphBackEdges = multimapInsert to from (graphBackEdges g)
  }

emptyGraph :: Graph
emptyGraph = Graph IntMap.empty IntMap.empty

fromEdges :: [(Int, Int)] -> Graph
fromEdges = foldr insertEdge emptyGraph

edgesFrom :: Graph -> Int -> [Int]
edgesFrom g v = fromMaybe [] (IntMap.lookup v (graphEdges g))
