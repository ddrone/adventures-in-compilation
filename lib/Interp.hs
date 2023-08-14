module Interp where

import Prelude hiding (lookup)
import Control.Arrow
import Control.Exception
import Control.Monad (join)
import Data.Map (Map)
import Data.IORef
import Data.Typeable
import qualified Data.Text as Text
import qualified Data.Map as Map

import AST
import Utils (exactZip)

data Value
  = IntV LangInt
  | BoolV Bool
  deriving (Show)

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
  | WrongBinType Binop
  | WrongUnaryType Unop
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

valueNum :: Binop -> Value -> IO LangInt
valueNum op v = case v of
  IntV n -> pure n
  _ -> throwIO (WrongBinType op)

valueBool :: Binop -> Value -> IO Bool
valueBool op v = case v of
  BoolV b -> pure b
  _ -> throwIO (WrongBinType op)

evalBinop :: Binop -> Value -> Value -> IO Value
evalBinop bop x y =
  let get = valueNum bop in
  let getB = valueBool bop in
  case bop of
    Add -> IntV <$> ((+) <$> get x <*> get y)
    Sub -> IntV <$> ((-) <$> get x <*> get y)
    Mul -> IntV <$> ((*) <$> get x <*> get y)
    Div -> IntV <$> (div <$> get x <*> get y)
    Mod -> IntV <$> (mod <$> get x <*> get y)
    Lt -> BoolV <$> ((<) <$> get x <*> get y)
    Le -> BoolV <$> ((<=) <$> get x <*> get y)
    Gt -> BoolV <$> ((>) <$> get x <*> get y)
    Ge -> BoolV <$> ((>=) <$> get x <*> get y)
    Equal -> BoolV <$> ((==) <$> get x <*> get y)
    And -> BoolV <$> ((&&) <$> getB x <*> getB y)
    Or -> BoolV <$> ((||) <$> getB x <*> getB y)

evalUnop :: Unop -> Value -> IO Value
evalUnop uop x = undefined

eval :: IORef EvalState -> Exp -> IO Value
eval stateRef exp = case exp of
  Var v -> lookup stateRef v
  Lit l -> pure (IntV l)
  Call fnName args -> do
    argValues <- mapM (eval stateRef) args
    fn <- fnLookup stateRef fnName
    apply stateRef fn argValues
  Bin op l r -> join (evalBinop op <$> eval stateRef l <*> eval stateRef r)
  Unary op e -> evalUnop op =<< eval stateRef e

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
