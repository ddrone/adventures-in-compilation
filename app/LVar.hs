module Main where

import LVar.Parser (program)
import qualified Data.Text.IO as TextIO
import System.Environment (getArgs)
import Data.Foldable (forM_)
import Text.Megaparsec (runParser, errorBundlePretty)
import LVar.Compiler (compileAll)
import LVar.X86 (printProgram)
import System.FilePath
import System.Process
import LVar.Typechecker (typecheckModule)

main = do
  files <- getArgs
  forM_ files $ \file -> do
    let assemblyName = replaceExtensions file "s"
    let binaryName = replaceExtensions file "exe"
    contents <- TextIO.readFile file
    case runParser program file contents of
      Left err -> putStrLn (errorBundlePretty err)
      Right p -> do
        print p
        case typecheckModule p of
          Nothing -> do
            putStrLn $ "Typecheck of " ++ file ++ " successful!"
            let instrs = compileAll p
            let code = printProgram instrs
            TextIO.writeFile assemblyName code
            callProcess "cc"
              [ "-m64"
              , "-o"
              , binaryName
              , assemblyName
              , "tests/runtime.o"
              ]
          Just e -> do
            putStrLn $ "Typecheck of " ++ file ++ " FAILED!"
            print e
