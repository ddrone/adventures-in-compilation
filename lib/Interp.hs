module Interp where

import Prelude hiding (lookup)
import Control.Arrow
import Control.Exception
import Data.Map (Map)
import Data.IORef
import Data.Typeable
import qualified Data.Text as Text
import qualified Data.Map as Map

import AST
import Utils (exactZip)

type Value = LangInt

type LocalEnv = Map Ident Value
type FnEnv = Map Ident Function

data EvalState = EvalState
  { esLocals :: LocalEnv
  , esFunctions :: FnEnv
  , esCurrentFunction :: Ident
  }

data EvalException
  = LookupFail Ident
  | FnLookupFail Ident
  | FnCallFail Ident Int Int
  | NoReturn Ident
  deriving (Typeable, Show)

instance Exception EvalException where

lookup :: IORef EvalState -> Ident -> IO Value
lookup stateRef name = do
  locals <- esLocals <$> readIORef stateRef
  case Map.lookup name locals of
    Nothing -> throwIO (LookupFail name)
    Just value -> pure value

fnLookup :: IORef EvalState -> Ident -> IO Function
fnLookup stateRef fnName = do
  funs <- esFunctions <$> readIORef stateRef
  case Map.lookup fnName funs of
    Nothing -> throwIO (FnLookupFail fnName)
    Just fun -> pure fun

apply :: IORef EvalState -> Function -> [Value] -> IO Value
apply stateRef fun args = do
  savedState <- readIORef stateRef
  -- Probably need to implement the evaluation of statemets first to be able to
  -- run functions, unfortunately.
  newLocals <- case Map.fromList <$> exactZip (fnArgs fun) args of
    Nothing -> throwIO (FnCallFail (fnName fun) (length (fnArgs fun)) (length args))
    Just env -> pure env
  writeIORef stateRef (savedState { esCurrentFunction = fnName fun, esLocals = newLocals })
  value <- evalBlock stateRef (fnBody fun)
  writeIORef stateRef savedState
  pure value

evalBinop :: Binop -> Value -> Value -> Value
evalBinop bop = case bop of
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div
  Mod -> mod

eval :: IORef EvalState -> Exp -> IO Value
eval stateRef exp = case exp of
  Var v -> lookup stateRef v
  Lit l -> pure l
  Call fnName args -> do
    argValues <- mapM (eval stateRef) args
    fn <- fnLookup stateRef fnName
    apply stateRef fn argValues
  Bin op l r -> evalBinop op <$> eval stateRef l <*> eval stateRef r

assign :: IORef EvalState -> Ident -> Value -> IO ()
assign stateRef var val = do
  localEnv <- esLocals <$> readIORef stateRef
  let newEnv = Map.insert var val localEnv
  modifyIORef stateRef $ \s -> s { esLocals = newEnv }

evalBlock :: IORef EvalState -> Block -> IO Value
evalBlock stateRef stmts = case stmts of
  [] -> do
    currFn <- esCurrentFunction <$> readIORef stateRef
    throwIO (NoReturn currFn)
  first : rest -> case first of
    Assign v e -> do
      value <- eval stateRef e
      assign stateRef v value
      evalBlock stateRef rest
    Return e -> eval stateRef e

initFnEnv :: Program -> FnEnv
initFnEnv = Map.fromList . map (fnName &&& id)

evalProgram :: Program -> IO Value
evalProgram program = do
  stateRef <- newIORef $
    EvalState
    { esLocals = Map.empty
    , esFunctions = initFnEnv program
    , esCurrentFunction = "<toplevel>"
    }
  eval stateRef (Call "main" [])
