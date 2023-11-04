module LVar.LexerDeps where

import Data.Aeson
import Data.Aeson.TH

data TokenInfo = TokenInfo
  { tokOffset :: Int
  , tokRow :: Int
  , tokColumn :: Int
  , tokEnd :: Int
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''TokenInfo)
