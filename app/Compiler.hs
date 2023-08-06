module Compiler where

import Prelude hiding (lookup)

import Control.Exception
import Data.IORef
import Data.Map (Map)
import Data.Typeable
import Data.Sequence (Seq, (|>))
import qualified Data.Map as Map

import AST (Ident)
import Instr (GenName)
import qualified AST
import qualified Instr
import qualified Data.Sequence as Sequence

data CompileState = CompileState
  { csLocals :: Map Ident GenName
  , csBlocks :: Map Ident GenName
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
