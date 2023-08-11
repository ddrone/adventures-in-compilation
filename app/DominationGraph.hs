module Main where

import Control.Monad (forM_)
import Debug.Trace (traceShowM)
import System.Environment (getArgs)
import Text.Megaparsec (runParser)
import qualified Data.Text.IO as TextIO
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Graph.Defs
import Graph.Parser
import Graph.DominatorTree
import Data.Maybe (fromJust)
import Graph.DominationFrontier (dominationFrontier, treeWithChildNodes)

printGraphWithDominatorTree :: ParsedGraph -> IO ()
printGraphWithDominatorTree pg = do
  let dt = dominatorTree pg
  let df = dominationFrontier pg
  let
    printVert x =
      TextIO.putStr (fromJust (IntMap.lookup x (pgNames pg)))
  let
    printEdgeCommon (x, y) = do
      putStr "  "
      printVert x
      putStr " -> "
      printVert y
    printEdge e = printEdgeCommon e >> putStrLn ""
    printIDom e = printEdgeCommon e >> putStrLn " [style=dashed,color=orange]"
    printDomFront e = printEdgeCommon e >> putStrLn " [style=dotted,color=blue]"
  putStrLn "digraph G {"
  mapM_ printEdge (allEdges (pgGraph pg))
  mapM_ printIDom (IntMap.toList dt)
  forM_ (IntMap.toList df) $ \(from, tos) -> do
    forM_ (IntSet.toList tos) $ \to -> do
      printDomFront (from, to)
  putStrLn "}"

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \file -> do
    contents <- TextIO.readFile file
    case runParser toplevelGraph file contents of
      Left e -> print e
      Right g -> printGraphWithDominatorTree g
