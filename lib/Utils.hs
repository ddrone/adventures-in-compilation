module Utils where

import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text

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

textRepeat :: Char -> Int -> Text
textRepeat c n = Text.pack (take n (repeat c))

rightPad :: Text -> Int -> Text
rightPad t len =
  let pad = max 0 (len - Text.length t)
  in t <> textRepeat ' ' pad

printTable :: [Text] -> [[Text]] -> Text
printTable headers rows =
  let headerLengths = map Text.length headers
      combineLengths xs ys =
        case (xs, ys) of
          (x : xs1, y : ys1) -> max x y : combineLengths xs1 ys1
          _ -> xs ++ ys
      addRow row lengths = combineLengths (map Text.length row) lengths
      maxLengths = foldr addRow headerLengths rows
      printRow row = Text.intercalate " | " (zipWith rightPad row maxLengths)
      afterHeader = Text.intercalate "-+-" (map (textRepeat '-') maxLengths)
  in Text.unlines (printRow headers : afterHeader : map printRow rows)
