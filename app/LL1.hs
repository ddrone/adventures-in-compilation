module Main where

import Prelude hiding (words)
import Data.Text (Text, words)
import qualified Grammar.Pregrammar as Pregrammar
import qualified Data.Text.IO as TextIO
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

main = case printLL1Table grammar "E" of
  Nothing -> putStrLn "error!"
  Just t -> TextIO.putStr t
