module UndirectedGraph where

import Data.Set (Set)
import SetMultimap (SetMultimap)
import qualified SetMultimap

newtype Graph v = Graph
  { graphEdges :: SetMultimap v v
  }
  deriving (Show)

addEdge :: Ord v => v -> v -> Graph v -> Graph v
addEdge from to (Graph g) =
  Graph (SetMultimap.insert from to (SetMultimap.insert to from g))

edgesFrom :: Ord v => v -> Graph v -> Set v
edgesFrom v (Graph g) = SetMultimap.lookup v g

empty :: Graph v
empty = Graph SetMultimap.empty