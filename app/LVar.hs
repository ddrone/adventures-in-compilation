module Main where

import Data.Foldable (forM_)
import System.FilePath
import System.Process
import System.Environment (getArgs)
import Text.Megaparsec (runParser, errorBundlePretty)
import qualified Data.Text.IO as TextIO

import LVar.AST (mapModule)
import LVar.ASTC (printModule)
import LVar.Compiler (compileAll, rcoModule, shrinkExpr)
import LVar.ExplicateControl (explicateControl)
import LVar.Parser (program)
import LVar.Typechecker (typecheckModule)
import LVar.X86 (printProgram)

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
            let clike = uncurry explicateControl (rcoModule (mapModule shrinkExpr p))
            TextIO.writeFile (replaceExtensions file "clike") (printModule clike)
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
