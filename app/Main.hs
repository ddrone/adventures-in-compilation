module Main where

import Prelude hiding (lookup)
import Control.Exception
import Data.Int (Int64)
import Data.Text (Text)
import Data.Map (Map)
import Data.IORef
import Data.Typeable
import qualified Data.Text as Text
import qualified Data.Map as Map

type LangInt = Int64
type Ident = Text

data Exp
  = Var Ident
  | Lit LangInt
  | Call Ident [Exp]

data Stmt
  = Assign Ident Exp
  | Return Exp

data Function = Function
  { fnName :: Ident
  , fnArgs :: [Ident]
  , fnBody :: Block
  }

type Block = [Stmt]

type Value = LangInt

type LocalEnv = Map Ident Value
type FnEnv = Map Ident Function

data EvalState = EvalState
  { esLocal :: LocalEnv
  , esFunctions :: FnEnv
  }

data EvalException
  = LookupFail Ident
  | FnLookupFail Ident
  | FnCallFail Ident Int Int
  deriving (Typeable, Show)

instance Exception EvalException where

lookup :: IORef EvalState -> Ident -> IO Value
lookup stateRef name = do
  locals <- esLocal <$> readIORef stateRef
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
  savedEnv <- esLocal <$> readIORef stateRef
  -- Probably need to implement the evaluation of statemets first to be able to
  -- run functions, unfortunately.
  _

eval :: IORef EvalState -> Exp -> IO Value
eval stateRef exp = case exp of
  Var v -> lookup stateRef v
  Lit l -> pure l
  Call fnName args -> do
    argValues <- mapM (eval stateRef) args
    fn <- fnLookup stateRef fnName
    apply stateRef fn argValues

main :: IO ()
main = putStrLn "Here be dragons"
