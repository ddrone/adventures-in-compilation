module Compiler where

import Prelude hiding (lookup)

import Control.Exception
import Data.IORef
import Data.Map (Map)
import Data.Typeable
import qualified Data.Map as Map

import AST (Ident)
import Instr (GenName)
import qualified AST
import qualified Instr

data CompileState = CompileState
  { csLocals :: Map Ident GenName
  , csBlocks :: Map Ident GenName
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

compile :: IORef CompileState -> AST.Exp -> IO Instr.Block
compile = undefined
