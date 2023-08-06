module Utils where

exactZip :: [a] -> [b] -> Maybe [(a, b)]
exactZip xs ys = case (xs, ys) of
  ([], []) -> Just []
  (x : xs, y : ys) -> ((x, y) :) <$> exactZip xs ys
  _ -> Nothing
