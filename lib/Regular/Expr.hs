module Regular.Expr where

import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

data Re
  = Char Char
  | Epsilon
  | Empty
  | Union Re Re
  | Concat Re Re
  | Iterate Re
  deriving (Show)

string :: String -> Re
string s = case s of
  [] -> Epsilon
  c : cs -> Concat (Char c) (string cs)

plus :: Re -> Re
plus r = Concat r (Iterate r)

optional :: Re -> Re
optional r = Union r Epsilon

range :: String -> Re
range s = case s of
  [] -> Empty
  c : cs -> Union (Char c) (range cs)

data EdgeLabel
  = EpsLabel
  | CharLabel Char
  deriving (Show)

data NFA = NFA
  { nfaCount :: Int
  , nfaEdges :: IntMap (Map EdgeLabel Int)
  }
  deriving (Show)
