module Main where

import Parser (program)
import qualified Data.Text.IO as TextIO
import System.Environment (getArgs)

main :: IO ()
main = do
  files <- getArgs
  mapM_ putStrLn files
