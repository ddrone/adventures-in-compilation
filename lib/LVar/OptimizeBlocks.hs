module LVar.OptimizeBlocks where

import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import LVar.ASTC
import Data.Maybe (fromJust)

countBlockUses :: Module -> IntMap Int
countBlockUses (Module start blocks) = do
  let getBlock id = fromJust (IntMap.lookup id blocks)
  let go map visited queue = case queue of
        [] -> map
        hd : tl ->
          if IntSet.member hd visited
            then go map visited tl
            else do
              let edgesOut = tailOuts (blockTail (getBlock hd))
              let newMap = foldr (\k -> IntMap.insertWith (+) k 1) map edgesOut
              go map (IntSet.insert hd visited) (edgesOut ++ queue)
  let startVerts = tailOuts (blockTail start)
  let startMap = IntMap.fromList (map (flip (,) 1) startVerts)
  go startMap IntSet.empty startVerts
