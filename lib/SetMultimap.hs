module SetMultimap where

import Prelude hiding (lookup)

import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype SetMultimap k v = SetMultimap { getMap :: Map k (Set v) }
  deriving (Show, Eq, Ord)

empty :: SetMultimap k v
empty = SetMultimap Map.empty

insert :: (Ord k, Ord v) => k -> v -> SetMultimap k v -> SetMultimap k v
insert key value (SetMultimap m) =
  SetMultimap (Map.insertWith Set.union key (Set.singleton value) m)

lookup :: Ord k => k -> SetMultimap k v -> Set v
lookup key (SetMultimap m) =
  fromMaybe Set.empty (Map.lookup key m)

lookupList :: Ord k => k -> SetMultimap k v -> [v]
lookupList key m = Set.toList (lookup key m)

fromList :: (Ord k, Ord v) => [(k, v)] -> SetMultimap k v
fromList = foldr (uncurry insert) SetMultimap.empty

fromSet :: (Ord k, Ord v) => Set (k, v) -> SetMultimap k v
fromSet = fromList . Set.toList
