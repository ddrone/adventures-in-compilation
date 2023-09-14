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

grammar = analyzeGrammar (fromPregrammar pg) "E"

main = do
  putStrLn "\n   Grammar\n"
  TextIO.putStr (printGrammar grammar)
  putStrLn "\n   Pretable\n"
  TextIO.putStr (printPreTable grammar)
  putStrLn "\n   Table\n"
  case printLL1Table grammar of
    Nothing -> putStrLn "error, must be multiple entries in pretable!"
    Just t -> TextIO.putStr t
