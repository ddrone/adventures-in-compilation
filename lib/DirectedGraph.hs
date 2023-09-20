module DirectedGraph where
import SetMultimap (SetMultimap)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified SetMultimap
import qualified Graphviz
import Data.Text (Text)
import qualified Data.Set as Set

newtype Graph v = Graph
  { graphEdges :: SetMultimap v v
  }
  deriving (Show)

allNodes :: Ord v => Graph v -> Set v
allNodes (Graph g) = Map.keysSet (SetMultimap.getMap g)

addEdge :: Ord v => v -> v -> Graph v -> Graph v
addEdge from to (Graph g) =
  Graph (SetMultimap.insert from to g)

allEdges :: Ord v => Graph v -> [(v, v)]
allEdges (Graph g) = do
  (from, tos) <- Map.toList (SetMultimap.getMap g)
  to <- Set.toList tos
  pure (from, to)

printGraph :: Ord v => (v -> v -> Graphviz.Edge) -> Graph v -> Text
printGraph genEdge g =
  let edges = map (uncurry genEdge) (allEdges g) in
  Graphviz.printDigraph [] edges

printGraphSimple :: Ord v => (v -> Text) -> Graph v -> Text
printGraphSimple printNode g =
  let genEdge v u = Graphviz.edge (printNode v) (printNode u) in
  printGraph genEdge g
