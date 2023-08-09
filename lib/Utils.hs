module Utils where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

exactZip :: [a] -> [b] -> Maybe [(a, b)]
exactZip xs ys = case (xs, ys) of
  ([], []) -> Just []
  (x : xs, y : ys) -> ((x, y) :) <$> exactZip xs ys
  _ -> Nothing

multimapInsert :: Int -> a -> IntMap [a] -> IntMap [a]
multimapInsert key value = IntMap.insertWith (++) key [value]
