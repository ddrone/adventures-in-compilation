module LVar.CompilerPipeline where

import Pipeline
import Data.Text (Text)
import LVar.Compiler (rcoModule, shrinkExpr, peModule, selectInstructions, assignHomesAndCountVars, AssignHomesResult (..), patchInstructions, generateWrapper)
import qualified Data.Text as Text
import qualified LVar.AST as AST
import LVar.Typechecker (typecheckModule, printTypeError)
import qualified LVar.ASTMon as ASTMon
import LVar.ExplicateControl (explicateControl)
import qualified LVar.ASTC as ASTC
import qualified DirectedGraph
import qualified LVar.X86 as X86
import LVar.OptimizeBlocks (optimizeModule)
import LVar.Lexer (scanTokens, tkError)
import LVar.ParserWrapper

compile :: FilePath -> Text -> Pipeline Text
compile filename source = do
  let tokens = scanTokens source
  case tkError tokens of
    Nothing -> pure ()
    Just err -> abort "Lexer error" -- TODO: also add the location here
  statements <-
    case runParser tokens of
      Left (_, err) -> abort (Text.pack err)
      Right stmts -> pure stmts
  let mod = AST.Module statements
  case typecheckModule mod of
    Nothing -> pure ()
    Just err -> abort (printTypeError err)
  let (rco, ecStart) = rcoModule (AST.mapModule shrinkExpr (AST.stripAnn mod))
  let pevaled = peModule rco
  emit "mon" (ASTMon.printModule pevaled)
  let explicated = optimizeModule (explicateControl pevaled ecStart)
  emit "clike" (ASTC.printModule explicated)
  let explicatedGraph = ASTC.toGraph explicated
  let topSort = DirectedGraph.topologicalSort explicatedGraph 0
  let selected = selectInstructions explicated
  let (AssignHomesResult count csr assigned) = assignHomesAndCountVars (DirectedGraph.map ASTC.printLabel explicatedGraph) (map ASTC.printLabel topSort) selected
  let patched = X86.mapProgramBlocks patchInstructions assigned
  let (prefix, suffix) = generateWrapper csr count
  pure (X86.printProgram prefix suffix patched)
