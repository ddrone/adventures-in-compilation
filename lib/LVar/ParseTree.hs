module LVar.ParseTree where

import Data.Aeson
import Data.Aeson.TH

import LVar.LexerDeps (TokenInfo)

type ParseForest = [ParseTree]

data ParseTree = ParseTree
  { ptName :: String
  , ptTokenInfo :: TokenInfo
  , ptChildren :: ParseForest
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ParseTree)

class ToParseForest a where
  toParseForest :: a -> ParseForest

class ToParseTree a where
  toParseTree :: a -> ParseTree

instance ToParseTree a => ToParseForest [a] where
  toParseForest = map toParseTree
