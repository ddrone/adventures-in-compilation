module Main where

import Data.Foldable (forM_)
import System.FilePath
import System.Process
import System.Environment (getArgs)
import qualified Data.Text.IO as TextIO

import LVar.CompilerPipeline (compile)
import qualified Pipeline

main = do
  files <- getArgs
  forM_ files $ \file -> do
    let assemblyName = replaceExtensions file "s"
    let binaryName = replaceExtensions file "exe"
    contents <- TextIO.readFile file
    assembly <- Pipeline.runIO file (compile file contents)
    TextIO.writeFile assemblyName assembly
    callProcess "cc"
      [ "-m64"
      , "-o"
      , binaryName
      , assemblyName
      , "tests/runtime.o"
      ]
