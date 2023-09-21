module Main where

import Data.Foldable (forM_)
import System.FilePath
import System.Process
import System.Environment (getArgs)
import Text.Megaparsec (runParser, errorBundlePretty)
import qualified Data.Text.IO as TextIO

import LVar.AST (mapModule)
import LVar.ASTC (printModule)
import LVar.Compiler (compileAll, rcoModule, shrinkExpr, selectInstructions)
import LVar.ExplicateControl (explicateControl)
import LVar.Liveness (computeLiveMap, printLiveBlockMap)
import LVar.Parser (program)
import LVar.Typechecker (typecheckModule)
import LVar.X86 (printProgram, progBlocks)
import qualified LVar.ASTMon as ASTMon
import qualified DirectedGraph
import qualified LVar.ASTC as ASTC

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
            let rco = rcoModule (mapModule shrinkExpr p)
            TextIO.writeFile (replaceExtensions file "mon") (ASTMon.printModule (fst rco))
            let clike = uncurry explicateControl rco
            TextIO.writeFile (replaceExtensions file "clike") (printModule clike)
            let explicatedGraph = ASTC.toGraph clike
            let topSort = DirectedGraph.topologicalSort explicatedGraph 0
            let queue = reverse (map ASTC.printLabel topSort)
            let selected = selectInstructions clike
            let map = computeLiveMap queue (DirectedGraph.map ASTC.printLabel explicatedGraph) (progBlocks selected)
            TextIO.writeFile (replaceExtension file "live") (printLiveBlockMap ASTMon.printName map)
            let code = compileAll p
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
