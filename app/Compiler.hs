module Compiler where

import Prelude hiding (lookup)

import Control.Exception
import Data.IORef
import Data.Map (Map)
import Data.Typeable
import Data.Sequence (Seq)
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

freshLocal :: IORef CompileState -> IO GenName
freshLocal = undefined

compile :: IORef CompileState -> IORef (Seq Instr.Assign) -> AST.Exp -> IO GenName
compile stateRef seqRef exp = case exp of
  AST.Var v -> lookupLocal stateRef v
  AST.Lit l -> undefined
  AST.Call fnName args -> undefined