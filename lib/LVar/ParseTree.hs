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
