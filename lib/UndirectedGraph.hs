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
import Data.Maybe (catMaybes)
import Control.Arrow ((&&&))

newtype Graph v = Graph
  { graphEdges :: SetMultimap v v
  }
  deriving (Show)

addEdge :: Ord v => v -> v -> Graph v -> Graph v
addEdge from to (Graph g) =
  Graph (SetMultimap.insert from to (SetMultimap.insert to from g))

edgesFrom :: Ord v => v -> Graph v -> Set v
edgesFrom v (Graph g) = SetMultimap.lookup v g

edgesList :: Ord v => v -> Graph v -> [v]
edgesList v g = Set.toList (edgesFrom v g)

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

catMaybesSet :: Ord a => [Maybe a] -> Set a
catMaybesSet = foldr (maybe id Set.insert) Set.empty

maximumBy :: Ord k => (a -> k) -> [a] -> Maybe a
maximumBy key ls =
  let go k a ls = case ls of
        [] -> Just a
        (a1 : rest) ->
          let k1 = key a1 in
          if k1 > k
            then go k1 a1 rest
            else go k a rest
  in
    case ls of
      [] -> Nothing
      (a : rest) -> go (key a) a rest

-- minimumExcludant takes a set of natural numbers and returns the least natural number
-- that is not in this list
minimumExcludant :: Set Int -> Int
minimumExcludant set = go 0 (Set.toAscList set)
  where
    go candidate ls = case ls of
      hd : tl | hd == candidate -> go (candidate + 1) tl
      _ -> candidate

saturationColoring :: Ord v => Set v -> Graph v -> Map v Int
saturationColoring nodes g =
  let iter remaining colors = do
        let excluded v = catMaybesSet (map (flip Map.lookup colors) (edgesList v g))
        let options = map (id &&& excluded) (Set.toList remaining)
        case maximumBy (Set.size . snd) options of
          Nothing -> colors
          Just (v, ex) ->
            let newColor = minimumExcludant ex in
            iter (Set.delete v remaining) (Map.insert v newColor colors)
  in iter nodes Map.empty
