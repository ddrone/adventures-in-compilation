module Regular.DFA where

import Control.Monad (guard)
import Control.Monad.ST
import Data.STRef
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Text (Text)
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Graphviz (attr, nodeA, edgeA, edge, printGraph)
import Regular.NFA (NFA(..), EdgeLabel(..))
import SetMultimap (SetMultimap)
import qualified SetMultimap

data DFA = DFA
  { dfaStart :: Int
  , dfaFinal :: IntSet
  , dfaCount :: Int
  , dfaEdges :: IntMap (Map Char Int)
  }
  deriving (Show)

next :: DFA -> Int -> Char -> Maybe Int
next dfa state c =
  let edges = fromMaybe Map.empty (IntMap.lookup state (dfaEdges dfa)) in
  Map.lookup c edges

printDFA :: DFA -> Text
printDFA dfa =
  let n :: Int -> Text
      n = Text.pack . show

      allNodes = IntSet.fromList [0..(dfaCount dfa) - 1]
      nonFinalNodes = IntSet.difference allNodes (dfaFinal dfa)

      nodes =
        nodeA "start" [attr "shape" "none"] :
        map (\x -> nodeA (n x) [attr "shape" "circle"]) (IntSet.toList nonFinalNodes) ++
        map (\x -> nodeA (n x) [attr "shape" "doublecircle"]) (IntSet.toList (dfaFinal dfa))

      label c = Text.pack [c]

      edges = do
        (from, fromEdges) <- IntMap.toList (dfaEdges dfa)
        (l, to) <- Map.toList fromEdges
        pure (edgeA (n from) (n to) [attr "label" (label l)])
  in printGraph nodes (edge "start" (n (dfaStart dfa)) : edges)

buildDFA :: NFA -> DFA
buildDFA nfa = runST $ do
  setMapping <- newSTRef (Map.empty :: Map IntSet Int)
  finalSet <- newSTRef IntSet.empty
  let nodeId set = do
        result <- Map.lookup set <$> readSTRef setMapping
        case result of
          Just v -> pure v
          Nothing -> do
            next <- Map.size <$> readSTRef setMapping
            modifySTRef setMapping (Map.insert set next)
            if IntSet.member (nfaEnd nfa) set
              then modifySTRef finalSet (IntSet.insert next)
              else pure ()
            pure next
  let edgesFrom from = fromMaybe Map.empty (IntMap.lookup from (nfaEdges nfa))
  let epsEdges node = fromMaybe [] (Map.lookup EpsLabel (edgesFrom node))
  let charEdgesFrom c node = fromMaybe [] (Map.lookup (CharLabel c) (edgesFrom node))
  let addEpsClosure node set =
        if IntSet.member node set
          then set
          else foldr addEpsClosure (IntSet.insert node set) (epsEdges node)
  let epsClosure node = addEpsClosure node IntSet.empty
  let epsClosureList nodes = foldr addEpsClosure IntSet.empty nodes

  let charEdges from =
        let allEdges = Map.keys (fromMaybe Map.empty (IntMap.lookup from (nfaEdges nfa)))
            fromLabel l = case l of
              CharLabel c -> Just c
              EpsLabel -> Nothing
        in Set.fromList (catMaybes (map fromLabel allEdges))
  let allChars set = Set.toList (mconcat (fmap charEdges set))
  let justSetEdge set c = mconcat (fmap (IntSet.fromList . charEdgesFrom c) set)
  let setEdge set c = epsClosureList (IntSet.toList (justSetEdge set c))

  dfaEdges <- newSTRef (IntMap.empty :: IntMap (Map Char Int))
  let go visited id set = do
        if IntSet.member id visited
          then pure visited
          else do
            let chars = allChars set
            let handleChar c = do
                  let next = setEdge set c
                  (,) c <$> nodeId next
            edges <- Map.fromList <$> mapM handleChar chars
            modifySTRef dfaEdges (IntMap.insert id edges)

            let nextSets = map (setEdge set) chars
            loop (IntSet.insert id visited) nextSets
      loop visited nexts =
        case nexts of
          [] -> pure visited
          first : rest -> do
            id <- nodeId first
            next <- go visited id (IntSet.toList first)
            loop next rest
  let startSet = epsClosure (nfaStart nfa)
  startId <- nodeId startSet
  go IntSet.empty startId (IntSet.toList startSet)

  count <- Map.size <$> readSTRef setMapping
  DFA startId <$> readSTRef finalSet <*> (Map.size <$> readSTRef setMapping) <*> readSTRef dfaEdges

-- In this formulation of DFA (where not all edges are stored, two vertices can be distinguished
-- initially not only if one of them is accepting and the other is not, but if one of them has an
-- edge the other does not (therefore terminating the scanning process)
--
-- Building the edges of the graph should not be affected though.
minimizeDFA :: DFA -> DFA
minimizeDFA dfa = do
  let verts = [0..(dfaCount dfa) - 1]
  let pairs = do
        e1 <- verts
        e2 <- verts
        guard (e1 /= e2)
        pure (e1, e2)
  let edgesTo u v = do
        let vEdges = fromMaybe Map.empty (IntMap.lookup v (dfaEdges dfa))
        (c, dest) <- Map.toList (fromMaybe Map.empty (IntMap.lookup u (dfaEdges dfa)))
        case Map.lookup c vEdges of
          Nothing -> []
          Just dest2 ->
            if dest == dest2
              then []
              else [(dest, dest2)]
  let allEdges = SetMultimap.fromList $ do
        p1@(u, v) <- pairs
        p2 <- edgesTo u v
        pure (p2, p1)

  let isBaseDistinguishable u v =
        let byFinal = IntSet.member u (dfaFinal dfa) /= IntSet.member v (dfaFinal dfa)
            outCharsU = fromMaybe Set.empty (Map.keysSet <$> IntMap.lookup u (dfaEdges dfa))
            outCharsV = fromMaybe Set.empty (Map.keysSet <$> IntMap.lookup v (dfaEdges dfa))
            uHasExtra = not (Set.null (Set.difference outCharsU outCharsV))
            vHasExtra = not (Set.null (Set.difference outCharsV outCharsU))
        in byFinal || uHasExtra || vHasExtra
  let initialQueue = filter (uncurry isBaseDistinguishable) pairs

  let go visited queue = case queue of
        [] -> visited
        first : rest ->
          if Set.member first visited
            then go visited rest
            else go (Set.insert first visited) (SetMultimap.lookupList first allEdges ++ rest)
  let equivalentPairs = Set.difference (Set.fromList pairs) (go Set.empty initialQueue)
  let equivalentMap = SetMultimap.fromSet equivalentPairs
  let enumerateClasses visited from
        | from >= dfaCount dfa = []
        | Set.member from visited = enumerateClasses visited (from + 1)
        | otherwise =
            let curr = Set.insert from (SetMultimap.lookup from equivalentMap) in
            curr : enumerateClasses (Set.union visited curr) (from + 1)
  let allClasses = zip [0..] (enumerateClasses Set.empty 0)
  let classMapping = IntMap.fromList $ do
        (classId, elems) <- allClasses
        elem <- Set.toList elems
        pure (elem, classId)

  let getClass elem = fromJust (IntMap.lookup elem classMapping)
  let newStart = getClass (dfaStart dfa)
  let classRep (classId, elems) = (classId, fromJust (Set.lookupMin elems))
  let allClassReps = map classRep allClasses

  let newFinals = IntSet.fromList (map fst (filter (flip IntSet.member (dfaFinal dfa) . snd) allClassReps))

  let edgesFrom (classId, rep) =
        let origEdges = fromMaybe Map.empty (IntMap.lookup rep (dfaEdges dfa)) in
        (classId, Map.map getClass origEdges)
  let newEdges = IntMap.fromList (map edgesFrom allClassReps)
  let newCount = length allClasses
  DFA newStart newFinals newCount newEdges