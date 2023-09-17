module UndirectedGraph where

import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import SetMultimap (SetMultimap)
import qualified SetMultimap
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (guard)
import qualified Graphviz

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

allEdges :: Ord v => Graph v -> [(v, v)]
allEdges (Graph g) = do
  (from, tos) <- Map.toList (SetMultimap.getMap g)
  to <- Set.toList tos
  guard (from <= to)
  pure (from, to)

printGraph :: Ord v => (v -> v -> Graphviz.Edge) -> Graph v -> Text
printGraph genEdge g =
  let edges = map (uncurry genEdge) (allEdges g) in
  Graphviz.printUndirectedGraph [] edges

printGraphSimple :: Ord v => (v -> Text) -> Graph v -> Text
printGraphSimple printNode g =
  let genEdge v u = Graphviz.edge (printNode v) (printNode u) in
  printGraph genEdge g
