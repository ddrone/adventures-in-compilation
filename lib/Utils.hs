module Utils where

import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text
import Control.Monad.State

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

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

concatSetsMap :: Ord b => (a -> Set b) -> [a] -> Set b
concatSetsMap f = foldr (Set.union . f) Set.empty

runLocal :: Monad m => StateT s m a -> StateT s m (a, s)
runLocal comp = do
  curr <- get
  lift (runStateT comp curr)
