module CFG.Compiler where

import Prelude hiding (lookup)

import Control.Exception
import Control.Monad (when)
import Data.IORef
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Typeable
import Data.Sequence (Seq, (|>))
import qualified Data.Map as Map

import AST (Ident)
import CFG.Instr (GenName)
import qualified AST
import qualified CFG.Instr as Instr
import qualified Data.Sequence as Sequence
import qualified CFG.Instr as Inst

data CompileState = CompileState
  { csBlocks :: Map Ident Int
  , csNextGen :: Int
  , csOutput :: Seq Instr.Block
  }

data CompileException
  = LookupFail Ident
  | FnLookupFail Ident
  | FnCallFail Ident Int Int
  | NoReturn Ident
  | UnreachableCode Ident
  deriving (Typeable, Show)

instance Exception CompileException

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
  AST.Var v -> pure (Instr.Src v)
  AST.Lit l -> do
    name <- freshLocal stateRef "lit"
    writeAssign seqRef name (Instr.Lit l)
  AST.Call fnName args -> do
    argNames <- traverse (compile stateRef seqRef) args
    name <- freshLocal stateRef "call"
    writeAssign seqRef name (Instr.Call fnName argNames)
  AST.Bin op l r -> do
    lname <- compile stateRef seqRef l
    rname <- compile stateRef seqRef r
    name <- freshLocal stateRef "bin"
    writeAssign seqRef name (Instr.Bin op lname rname)
  AST.Unary op e -> do
    innerName <- compile stateRef seqRef e
    name <- freshLocal stateRef "unary"
    writeAssign seqRef name (Instr.Unary op name)

writeBlock
  :: IORef CompileState
  -> IORef (Seq Instr.Assign)
  -> Instr.BlockName
  -> Instr.BlockEnd
  -> IO Bool
writeBlock stateRef seqRef name ret = do
  body <- toList <$> readIORef seqRef
  writeIORef seqRef Sequence.empty
  modifyIORef stateRef $ \s -> s { csOutput = csOutput s |> Instr.Block name body ret }
  pure True

compileBlock
  :: Ident
  -> IORef CompileState
  -> IORef (Seq Instr.Assign)
  -> Instr.BlockName
  -> AST.Block
  -> IO Bool
compileBlock fnName stateRef seqRef blockName stmts =
  case stmts of
    [] -> pure False
    (first : rest) -> case first of
      AST.Assign v exp -> do
        name <- compile stateRef seqRef exp
        writeAssign seqRef (Instr.Src v) (Instr.Var name)
        compileBlock fnName stateRef seqRef blockName rest
      AST.Return exp -> do
        name <- compile stateRef seqRef exp
        writeBlock stateRef seqRef blockName (Instr.Ret name)
        compileBlock fnName stateRef seqRef blockName rest
      AST.If cond cons alt -> do
        name <- compile stateRef seqRef cond
        consRef <- newIORef Sequence.empty
        consName <- freshBlockName stateRef fnName
        consDone <- compileBlock fnName stateRef consRef consName cons
        altRef <- newIORef Sequence.empty
        altName <- freshBlockName stateRef fnName
        altDone <- compileBlock fnName stateRef altRef altName alt
        writeBlock stateRef seqRef blockName (Instr.CondJump name consName altName)
        if consDone && altDone
          then case rest of
            [] -> pure True
            _ -> throwIO (UnreachableCode fnName)
          else do
            nextBlock <- freshBlockName stateRef fnName
            when (not consDone) $ do
              writeBlock stateRef consRef consName (Inst.Jump nextBlock)
              pure ()
            when (not altDone) $ do
              writeBlock stateRef altRef altName (Inst.Jump nextBlock)
              pure ()
            compileBlock fnName stateRef seqRef nextBlock rest

compileFunction
  :: IORef CompileState
  -> IORef (Seq Instr.Assign)
  -> AST.Function
  -> IO Instr.Function
compileFunction stateRef seqRef (AST.Function name args body) = do
  startBlock <- freshBlockName stateRef name
  done <- compileBlock name stateRef seqRef startBlock body
  blocks <- toList . csOutput <$> readIORef stateRef
  if done
    -- Probably need to clear output blocks as well
    then pure (Instr.Function name args blocks)
    else throwIO (NoReturn name)

-- TODO: implement the function
compileToplevel :: AST.Program -> IO [Instr.Function]
compileToplevel fns = do
  cs <- newIORef $ CompileState
    { csBlocks = Map.empty
    , csNextGen = 0
    , csOutput = Sequence.empty
    }
  seq <- newIORef Sequence.empty
  mapM (compileFunction cs seq) fns
