module Main where

import LVar.Parser (program)
import qualified Data.Text.IO as TextIO
import System.Environment (getArgs)
import Data.Foldable (forM_)
import Text.Megaparsec (runParser)

main = do
  files <- getArgs
  forM_ files $ \file -> do
    contents <- TextIO.readFile file
    case runParser program file contents of
      Left err -> print err
      Right p -> do
        print p
