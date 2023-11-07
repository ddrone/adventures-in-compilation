module Main where

import qualified Data.Text.IO as TextIO
import LVar.Lexer

main = do
  s <- TextIO.getContents
  print (scanTokens s)
