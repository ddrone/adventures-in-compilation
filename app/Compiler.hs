module Main where

import System.Environment (getArgs)
import Data.Foldable (forM_)
import Data.List (intersperse)
import Text.Megaparsec (runParser)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import CFG.Compiler (compileToplevel)
import CFG.Printer (printFunction)
import Parser (program)

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \file -> do
    contents <- TextIO.readFile file
    case runParser program file contents of
      Left err -> print err
      Right p -> do
        fns <- compileToplevel p
        let lines = concat $ intersperse [Text.pack ""] $ map printFunction fns
        TextIO.putStrLn (Text.unlines lines)
