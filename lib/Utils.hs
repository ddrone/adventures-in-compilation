module Utils where

import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

exactZip :: [a] -> [b] -> Maybe [(a, b)]
exactZip xs ys = case (xs, ys) of
  ([], []) -> Just []
  (x : xs, y : ys) -> ((x, y) :) <$> exactZip xs ys
  _ -> Nothing

multimapInsert :: Int -> a -> IntMap [a] -> IntMap [a]
multimapInsert key value = IntMap.insertWith (++) key [value]

multimapFromList :: [(Int, a)] -> IntMap [a]
multimapFromList = foldr (uncurry multimapInsert) IntMap.empty

regularMultimapInsert :: Ord k => k -> a -> Map k [a] -> Map k [a]
regularMultimapInsert key value = Map.insertWith (++) key [value]

untilEqual :: Eq a => (a -> a) -> a -> a
untilEqual iter start = go start (iter start)
  where
    go curr next =
      if curr == next
        then curr
        else go next (iter next)
