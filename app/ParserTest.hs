module Main where

import LVar.Lexer
import LVar.NewParser

main = do
  s <- getContents
  let lexemes = alexScanTokens s
  print (parse lexemes)
