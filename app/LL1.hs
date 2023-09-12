module Main where

import Prelude hiding (words)
import Data.Text (Text, words)
import qualified Grammar.Pregrammar as Pregrammar
import Grammar.Grammar

(&>) = Pregrammar.Rule
from &>> to = from &> words to

pg :: Pregrammar.Grammar
pg =
  [ "E" &>> "E' Econt"
  , "Econt" &> []
  , "Econt" &>> "+ E' Econt"
  , "E'" &>> "int"
  , "E'" &>> "( E )"
  ]

grammar = fromPregrammar pg

main = print (ll1Table grammar "E")
