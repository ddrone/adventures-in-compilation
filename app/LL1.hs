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

pg2 :: Pregrammar.Grammar
pg2 =
  [ "E" &>> "List"
  , "E" &>> "Object"
  , "E" &>> "int"
  , "List" &>> "[ ListItems ]"
  , "ListItems" &>> "E , ListItems"
  , "ListItems" &>> ""
  , "Object" &>> "{ ObjectPairs }"
  , "ObjectPairs" &>> "int : E , ObjectPairs"
  , "ObjectPairs" &>> ""
  ]

grammar = analyzeGrammar (fromPregrammar pg2) "E"

main = do
  putStrLn "\n   Grammar\n"
  TextIO.putStr (printGrammar grammar)
  putStrLn "\n   Pretable\n"
  TextIO.putStr (printPreTable grammar)
  putStrLn "\n   Table\n"
  case printLL1Table grammar of
    Nothing -> putStrLn "error, must be multiple entries in pretable!"
    Just t -> TextIO.putStr t
