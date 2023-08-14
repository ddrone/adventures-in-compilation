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
  | WrongCondition
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

valueNum :: EvalException -> Value -> IO LangInt
valueNum exn v = case v of
  IntV n -> pure n
  _ -> throwIO exn

valueBool :: EvalException -> Value -> IO Bool
valueBool exn v = case v of
  BoolV b -> pure b
  _ -> throwIO exn

evalBinop :: Binop -> Value -> Value -> IO Value
evalBinop bop x y =
  let get = valueNum (WrongBinType bop) in
  let getB = valueBool (WrongBinType bop) in
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
evalUnop uop x =
  let get = valueNum (WrongUnaryType uop) in
  let getB = valueBool (WrongUnaryType uop) in
  case uop of
    Not -> BoolV . not <$> getB x
    Neg -> IntV . negate <$> get x

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
    If cond cons alt -> do
      value <- eval stateRef cond
      condValue <- case value of
        BoolV v -> pure v
        _ -> throwIO WrongCondition
      if condValue
        then evalBlock stateRef (cons ++ rest)
        else evalBlock stateRef (alt ++ rest)

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
