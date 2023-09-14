module Regular.Expr where

import Control.Monad.ST
import Data.STRef
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (fromMaybe, catMaybes)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

data Re
  = Char Char
  | Epsilon
  | Empty
  | Union Re Re
  | Concat Re Re
  | Iterate Re
  deriving (Show)

string :: String -> Re
string s = case s of
  [] -> Epsilon
  c : cs -> Concat (Char c) (string cs)

plus :: Re -> Re
plus r = Concat r (Iterate r)

optional :: Re -> Re
optional r = Union r Epsilon

range :: String -> Re
range s = case s of
  [] -> Empty
  c : cs -> Union (Char c) (range cs)

data EdgeLabel
  = EpsLabel
  | CharLabel Char
  deriving (Show, Eq, Ord)

data NFA = NFA
  { nfaStart :: Int
  , nfaEnd :: Int
  , nfaCount :: Int
  , nfaEdges :: IntMap (Map EdgeLabel [Int])
  }
  deriving (Show)

buildNFA :: Re -> NFA
buildNFA re = runST $ do
  next <- newSTRef 0
  let fresh = do
        result <- readSTRef next
        writeSTRef next (result + 1)
        pure result
  edges <- newSTRef (IntMap.empty :: IntMap (Map EdgeLabel [Int]))
  let addEdge from label to = do
        let add = Map.singleton label to
        modifySTRef edges (IntMap.insertWith (Map.unionWith (++)) from (Map.singleton label [to]))
  let addEps from to = addEdge from EpsLabel to
  let go expr = do
        start <- fresh
        end <- fresh
        case expr of
          Char c -> do
            addEdge start (CharLabel c) end
          Epsilon -> do
            addEps start end
          Empty -> pure () -- nothing to insert
          Union r1 r2 -> do
            (s1, e1) <- go r1
            (s2, e2) <- go r2
            addEps start s1
            addEps start s2
            addEps e1 end
            addEps e2 end
          Concat r1 r2 -> do
            (s1, e1) <- go r1
            (s2, e2) <- go r2
            addEps start s1
            addEps e1 s2
            addEps s2 end
          Iterate r -> do
            (s, e) <- go r
            addEps start s
            addEps e start
            addEps e end
            addEps start end
        pure (start, end)
  (start, end) <- go re
  NFA start end <$> readSTRef next <*> readSTRef edges

data DFA = DFA
  { dfaStart :: Int
  , dfaFinal :: IntSet
  , dfaCount :: Int
  , dfaEdges :: IntMap (Map Char Int)
  }
  deriving (Show)

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

  let charEdges from =
        let allEdges = Map.keys (fromMaybe Map.empty (IntMap.lookup from (nfaEdges nfa)))
            fromLabel l = case l of
              CharLabel c -> Just c
              EpsLabel -> Nothing
        in Set.fromList (catMaybes (map fromLabel allEdges))
  let allChars set = Set.toList (mconcat (fmap charEdges set))
  let setEdge set c = mconcat (fmap (IntSet.fromList . charEdgesFrom c) set)

  dfaEdges <- newSTRef (IntMap.empty :: IntMap (Map Char Int))
  let go visited id set = do
        if IntSet.member id visited
          then pure ()
          else do
            let chars = allChars set
            let handleChar c = do
                  let next = setEdge set c
                  (,) c <$> nodeId next
            edges <- Map.fromList <$> mapM handleChar chars
            modifySTRef dfaEdges (IntMap.insert id edges)

            let nextSets = map (setEdge set) chars
            loop visited nextSets
      loop visited nexts =
        case nexts of
          [] -> pure ()
          first : rest -> do
            id <- nodeId first
            go visited id (IntSet.toList first)
            loop (IntSet.insert id visited) rest
  let startSet = epsClosure (nfaStart nfa)
  startId <- nodeId startSet
  go IntSet.empty startId (IntSet.toList startSet)

  count <- Map.size <$> readSTRef setMapping
  DFA startId <$> readSTRef finalSet <*> (Map.size <$> readSTRef setMapping) <*> readSTRef dfaEdges
