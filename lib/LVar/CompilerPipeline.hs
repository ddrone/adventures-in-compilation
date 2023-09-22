module LVar.CompilerPipeline where

import Pipeline
import Data.Text (Text)
import LVar.Compiler (rcoModule, shrinkExpr, peModule, selectInstructions, assignHomesAndCountVars, AssignHomesResult (..), patchInstructions, generateWrapper)
import Text.Megaparsec (runParser, errorBundlePretty)
import LVar.Parser (program)
import qualified Data.Text as Text
import qualified LVar.AST as AST
import LVar.Typechecker (typecheckModule)
import qualified LVar.ASTMon as ASTMon
import LVar.ExplicateControl (explicateControl)
import qualified LVar.ASTC as ASTC
import qualified DirectedGraph
import qualified LVar.X86 as X86

compile :: FilePath -> Text -> Pipeline Text
compile filename source = do
  mod <- case runParser program filename source of
    Left err -> abort (Text.pack (errorBundlePretty err))
    Right m -> pure m
  case typecheckModule mod of
    Nothing -> pure ()
    -- TODO: write a function to pretty-print typechecker error
    Just err -> abort (Text.pack (show err))
  let (rco, ecStart) = rcoModule (AST.mapModule shrinkExpr mod)
  let pevaled = peModule rco
  emit "mon" (ASTMon.printModule pevaled)
  let explicated = explicateControl pevaled ecStart
  emit "clike" (ASTC.printModule explicated)
  let explicatedGraph = ASTC.toGraph explicated
  let topSort = DirectedGraph.topologicalSort explicatedGraph 0
  let selected = selectInstructions explicated
  let (AssignHomesResult count csr assigned) = assignHomesAndCountVars (DirectedGraph.map ASTC.printLabel explicatedGraph) (map ASTC.printLabel topSort) selected
  let patched = X86.mapProgramBlocks patchInstructions assigned
  let (prefix, suffix) = generateWrapper csr count
  pure (X86.printProgram prefix suffix patched)
