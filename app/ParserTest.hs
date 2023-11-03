module Main where

import LVar.Lexer
import LVar.NewParser

main = do
  s <- getContents
  let lexemes = map snd (alexScanTokens s)
  print (parse lexemes)
