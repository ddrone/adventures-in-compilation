module CFG.Compiler where

import Prelude hiding (lookup)

import Control.Exception
import Data.IORef
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Typeable
import Data.Sequence (Seq, (|>))
import qualified Data.Map as Map

import AST (Ident)
import CFG.Instr (GenName)
import qualified AST
import qualified CFG.Instr as Instr
import qualified Data.Sequence as Sequence

data CompileState = CompileState
  { csLocals :: Map Ident GenName
  , csBlocks :: Map Ident Int
  , csNextGen :: Int
  }

data CompileException
  = LookupFail Ident
  | FnLookupFail Ident
  | FnCallFail Ident Int Int
  deriving (Typeable, Show)

instance Exception CompileException

lookupLocal :: IORef CompileState -> Ident -> IO GenName
lookupLocal stateRef name = do
  locals <- csLocals <$> readIORef stateRef
  case Map.lookup name locals of
    Nothing -> throwIO (LookupFail name)
    Just value -> pure value

freshLocal :: IORef CompileState -> Ident -> IO GenName
freshLocal stateRef hint = do
  id <- csNextGen <$> readIORef stateRef
  modifyIORef stateRef $ \s -> s { csNextGen = id + 1 }
  pure (Instr.Gen hint id)

freshBlockName :: IORef CompileState -> Ident -> IO Instr.BlockName
freshBlockName stateRef hint = do
  maybeId <- Map.lookup hint . csBlocks <$> readIORef stateRef
  let actualId = maybe 0 (+ 1) maybeId
  modifyIORef stateRef $ \s -> s { csBlocks = Map.insert hint actualId (csBlocks s) }
  pure (Instr.BlockName hint actualId)

writeAssign :: IORef (Seq Instr.Assign) -> GenName -> Instr.AssignSource -> IO GenName
writeAssign seqRef target source = do
  modifyIORef seqRef (|> Instr.Assign target source)
  pure target

compile :: IORef CompileState -> IORef (Seq Instr.Assign) -> AST.Exp -> IO GenName
compile stateRef seqRef exp = case exp of
  AST.Var v -> lookupLocal stateRef v
  AST.Lit l -> do
    name <- freshLocal stateRef "lit"
    writeAssign seqRef name (Instr.Lit l)
  AST.Call fnName args -> do
    argNames <- traverse (compile stateRef seqRef) args
    name <- freshLocal stateRef "call"
    writeAssign seqRef name (Instr.Call fnName argNames)

compileStmt
  :: Ident
  -> IORef CompileState
  -> IORef (Seq Instr.Assign)
  -> AST.Stmt
  -> IO (Maybe Instr.Block)
compileStmt fnName stateRef seqRef stmt = case stmt of
  AST.Assign v exp -> do
    name <- compile stateRef seqRef exp
    writeAssign seqRef (Instr.Src v) (Instr.Var name)
    pure Nothing
  AST.Return exp -> do
    name <- compile stateRef seqRef exp
    blockName <- freshBlockName stateRef fnName
    assigns <- readIORef seqRef
    pure $ Just (Instr.Block blockName (toList assigns) (Instr.Ret name))
