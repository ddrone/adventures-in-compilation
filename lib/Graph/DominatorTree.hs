module Graph.DominatorTree where

import Control.Monad (unless)
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.STRef
import GHC.ST (runST)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Graph.Defs

-- Kind of least common ancestor, but really bad & inefficient implementation
lca :: Int -> [Int] -> [Int] -> Int
lca curr xs ys = case (xs, ys) of
  (x : xs1, y : ys1) | x == y -> lca x xs1 ys1
  _ -> curr

commonSubseq :: Eq a => [a] -> [a] -> [a]
commonSubseq xs ys = case (xs, ys) of
  (x : xs, y : ys) | x == y -> x : commonSubseq xs ys
  _ -> []

dominatorTree :: Int -> Graph -> IntMap Int
dominatorTree root g = runST $ do
  parent <- newSTRef IntMap.empty
  let getParent v = IntMap.lookup v <$> readSTRef parent
  let parentSequence v = do
        next <- getParent v
        case next of
          Nothing -> pure [root]
          Just n -> (n :) <$> parentSequence n
  let allDominators v = do
        let prevs = edgesTo g v
        prevParents <- map reverse <$> traverse parentSequence prevs
        pure (foldr1 commonSubseq prevParents)
  let computeParent v = do
        let prevs = edgesTo g v
        case prevs of
          [single] -> pure single
          _ -> head . reverse <$> allDominators v
  let updateParent v = do
        computed <- computeParent v
        modifySTRef parent (IntMap.insert v computed)

  visited <- newSTRef IntSet.empty
  let
    bfs (v : rest) = do
      skip <- IntSet.member v <$> readSTRef visited
      if skip
        then pure ()
        else do
          modifySTRef visited (IntSet.insert v)
          unless (v == root) $ updateParent v
          bfs (rest ++ edgesFrom g v)
    bfs [] = pure ()
  bfs [root]
  readSTRef parent
