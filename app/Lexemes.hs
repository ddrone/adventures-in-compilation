module Main where

import LVar.Lexer

main = do
  s <- getContents
  print (scanTokens s)
