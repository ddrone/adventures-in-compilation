module Regular.NFA where

import Control.Monad.ST
import Data.STRef
import Data.List (delete)
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Graphviz (attr, nodeA, edgeA, edge, printGraph)
import Regular.Expr
import Control.Monad (forM)
import Control.Arrow ((***), (&&&))

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

data LabeledNFA l = LNFA
  { lnfaStart :: IntSet
  , lnfaEnd :: IntMap l
  , lnfaCount :: Int
  , lnfaEdges :: IntMap (Map EdgeLabel [Int])
  }

printNFA :: NFA -> Text
printNFA nfa =
  let n :: Int -> Text
      n = Text.pack . show
      nodes =
        nodeA "start" [attr "shape" "none"] :
        nodeA (n (nfaEnd nfa)) [attr "shape" "doublecircle"] :
        map (\x -> nodeA (n x) [attr "shape" "circle"]) (delete (nfaEnd nfa) [0..(nfaCount nfa) - 1])

      label l = case l of
        EpsLabel -> "ε"
        CharLabel c -> Text.pack [c]

      edges = do
        (from, fromEdges) <- IntMap.toList (nfaEdges nfa)
        (l, tos) <- Map.toList fromEdges
        to <- tos
        pure (edgeA (n from) (n to) [attr "label" (label l)])
  in printGraph nodes (edge "start" (n (nfaStart nfa)) : edges)

nfaBuildHelper edges next re = do
  let fresh = do
        result <- readSTRef next
        writeSTRef next (result + 1)
        pure result
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
            addEps e2 end
          Iterate r -> do
            (s, e) <- go r
            addEps start s
            addEps e start
            addEps e end
            addEps start end
        pure (start, end)
  go re

buildNFA :: Re -> NFA
buildNFA re = runST $ do
  next <- newSTRef 0
  edges <- newSTRef (IntMap.empty :: IntMap (Map EdgeLabel [Int]))
  (start, end) <- nfaBuildHelper edges next re
  NFA start end <$> readSTRef next <*> readSTRef edges

data LexicalSpecPart l = LexicalSpecPart
  { lspStart :: Int
  , lspEnd :: Int
  , lspLabel :: l
  }

buildLabeledNFA :: LexicalSpec l -> LabeledNFA l
buildLabeledNFA spec = runST $ do
  next <- newSTRef 0
  edges <- newSTRef (IntMap.empty :: IntMap (Map EdgeLabel [Int]))
  parts <- forM (Map.toList spec) $ \(label, re) -> do
    (start, end) <- nfaBuildHelper edges next re
    pure (LexicalSpecPart start end label)
  let start = IntSet.fromList (map lspStart parts)
  let end = IntMap.fromList (map (lspEnd &&& lspLabel) parts)
  LNFA start end <$> readSTRef next <*> readSTRef edges
