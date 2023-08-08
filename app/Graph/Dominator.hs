module Graph.Dominator where

import Control.Monad (mapM_)
import Data.IntSet (IntSet)
import Data.STRef
import GHC.ST (runST)
import qualified Data.IntSet as IntSet

import Graph.Defs

-- In order for vertex A to dominate vertex B, vertex B has to be accessible only
-- by going through vertex A (starting traversal from some particular root node)

dominates :: Int -> Graph -> Int -> Int -> Bool
dominates root g s t = runST $ do
  visited <- newSTRef IntSet.empty
  let isVisited v = IntSet.member v <$> readSTRef visited
  let markVisited v = modifySTRef visited (IntSet.insert v)
  let
    dfs v = do
      skip <- (v == s ||) <$> isVisited v
      if skip
        then pure ()
        else do
          markVisited v
          mapM_ dfs (edgesFrom g v)
  dfs root
  isVisited t
