module DirectedGraph where
import SetMultimap (SetMultimap)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified SetMultimap
import qualified Graphviz
import Data.Text (Text)
import qualified Data.Set as Set
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, modifySTRef)
import Control.Monad (when, unless)
import Control.Arrow ((***))

newtype Graph v = Graph
  { graphEdges :: SetMultimap v v
  }
  deriving (Show)

empty :: Graph v
empty = Graph SetMultimap.empty

allNodes :: Ord v => Graph v -> Set v
allNodes (Graph g) = Map.keysSet (SetMultimap.getMap g)

addEdge :: Ord v => v -> v -> Graph v -> Graph v
addEdge from to (Graph g) =
  Graph (SetMultimap.insert from to g)

edgesFrom :: Ord v => v -> Graph v -> Set v
edgesFrom v (Graph g) = SetMultimap.lookup v g

edgesList :: Ord v => v -> Graph v -> [v]
edgesList v g = Set.toList (edgesFrom v g)

allEdges :: Ord v => Graph v -> [(v, v)]
allEdges (Graph g) = do
  (from, tos) <- Map.toList (SetMultimap.getMap g)
  to <- Set.toList tos
  pure (from, to)

fromList :: Ord v => [(v, v)] -> Graph v
fromList = foldr (uncurry addEdge) empty

map :: (Ord v, Ord u) => (v -> u) -> Graph v -> Graph u
map f = fromList . Prelude.map (f *** f) . allEdges

printGraph :: Ord v => (v -> v -> Graphviz.Edge) -> Graph v -> Text
printGraph genEdge g =
  let edges = Prelude.map (uncurry genEdge) (allEdges g) in
  Graphviz.printDigraph [] edges

printGraphSimple :: Ord v => (v -> Text) -> Graph v -> Text
printGraphSimple printNode g =
  let genEdge v u = Graphviz.edge (printNode v) (printNode u) in
  printGraph genEdge g

topologicalSort :: Ord v => Graph v -> v -> [v]
topologicalSort g start = runST $ do
  result <- newSTRef []
  visited <- newSTRef Set.empty
  let go v = do
        isVisited <- Set.member v <$> readSTRef visited
        unless isVisited $ do
          modifySTRef visited (Set.insert v)
          mapM_ go (edgesFrom v g)
          modifySTRef result (v :)
  go start
  readSTRef result
