module Regular.Expr where

import Control.Monad.ST
import Data.STRef
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
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
  , dfaEnd :: Int
  , dfaCount :: Int
  , dfaEdges :: IntMap (Map Char Int)
  }
  deriving (Show)

buildDFA :: NFA -> DFA
buildDFA nfa = runST $ do
  setMapping <- newSTRef (Map.empty :: Map IntSet Int)
  let nodeId set = do
        result <- Map.lookup set <$> readSTRef setMapping
        case result of
          Just v -> pure v
          Nothing -> do
            next <- Map.size <$> readSTRef setMapping
            modifySTRef setMapping (Map.insert set next)
            pure next
  let edgesFrom from = fromMaybe Map.empty (IntMap.lookup from (nfaEdges nfa))
  let epsEdges node = fromMaybe [] (Map.lookup EpsLabel (edgesFrom node))
  let addEpsClosure node set =
        if IntSet.member node set
          then set
          else foldr addEpsClosure (IntSet.insert node set) (epsEdges node)
  let epsClosure node = addEpsClosure node IntSet.empty
  undefined -- TODO: continue here
