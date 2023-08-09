module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)
import Text.Megaparsec (runParser)
import qualified Data.Text.IO as TextIO

import Graph.Parser

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \file -> do
    contents <- TextIO.readFile file
    case runParser toplevelGraph file contents of
      Left e -> print e
      Right g -> printGraphWithDominatorTree g
