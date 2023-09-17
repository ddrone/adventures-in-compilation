module Main where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowM)
import System.Environment (getArgs)
import Text.Megaparsec (runParser)
import qualified Data.Text.IO as TextIO
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Graph.Defs
import Graph.Parser
import Graph.DominatorTree
import Graphviz (attr)
import qualified Graphviz
import Graph.DominationFrontier (dominationFrontier, treeWithChildNodes)

printGraphWithDominatorTree :: ParsedGraph -> IO ()
printGraphWithDominatorTree pg = do
  let dt = dominatorTree pg
  let df = dominationFrontier pg
  let
    printVert x =
      TextIO.putStr (fromJust (IntMap.lookup x (pgNames pg)))
    vertName x = fromJust (IntMap.lookup x (pgNames pg))
    edgeA attrs (x, y) = Graphviz.edgeA (vertName x) (vertName y) attrs
    edge = edgeA []
    edgeIdom = edgeA [attr "style" "dashed", attr "color" "orange"]
    edgeDomFront = edgeA [attr "style" "dotted", attr "color" "blue"]

    edges = map edge (allEdges (pgGraph pg))
    idomEdges = map edgeIdom (IntMap.toList dt)
    domFrontEdges = do
      (from, tos) <- IntMap.toList df
      to <- IntSet.toList tos
      pure (edgeDomFront (from, to))
  TextIO.putStrLn (Graphviz.printDigraph [] (edges ++ idomEdges ++ domFrontEdges))

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \file -> do
    contents <- TextIO.readFile file
    case runParser toplevelGraph file contents of
      Left e -> print e
      Right g -> printGraphWithDominatorTree g
