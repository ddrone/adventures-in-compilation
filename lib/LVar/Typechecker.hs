module LVar.Typechecker where

import LVar.Lexer (TokenInfo)
import LVar.NewParser (Block)
import LVar.AST (Expr (..), Binop (..), Unop (..), Stmt (..), Module, GenModule (Module))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when)
import qualified Data.Text as Text

data Type
  = Int64T
  | BoolT
  deriving (Show, Eq, Ord)

data TypeError a = TypeError
  { teSource :: a
  , teReason :: Text
  }
  deriving (Show, Functor)

data BinopTy = BinopTy
  { btArg :: Type
  , btResult :: Type
  }

binopTy :: Binop -> BinopTy
binopTy = \case
  Add -> int `to` int
  Sub -> int `to` int
  Le -> int `to` bool
  Lt -> int `to` bool
  Ge -> int `to` bool
  Gt -> int `to` bool
  Eq -> error "should be checked specially"
  Ne -> error "should be checked specially"
  And -> bool `to` bool
  Or -> bool `to` bool
  where
    int = Int64T
    bool = BoolT
    to = BinopTy

data UnopTy = UnopTy
  { utArg :: Type
  , utResult :: Type
  }

unopTy :: Unop -> UnopTy
unopTy = \case
  Not -> bool `to` bool
  Neg -> int `to` int
  where
    int = Int64T
    bool = BoolT
    to = UnopTy

type TyEnv = Map Text Type

typecheckExpr :: TyEnv -> Expr a -> Either (TypeError (Expr a)) Type
typecheckExpr env expr = case expr of
  Const _ -> pure Int64T
  Bool _ -> pure BoolT
  Name n -> case Map.lookup n env of
    Just t -> pure t
    Nothing -> tyErr "usage of undefined variable"
  Bin op e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case op of
      Eq -> do
        when (t1 /= t2) $
          tyErr "both sides of equality comparison should have equal types"
        pure BoolT
      Ne -> do
        when (t1 /= t2) $
          tyErr "both sides of inequality comparison should have equal types"
        pure BoolT
      _ -> do
        let (BinopTy from to) = binopTy op
        when (from /= t1) $
          tyErr "left argument of binary operator has the wrong type"
        when (from /= t2) $
          tyErr "right argument of binary operator has the wrong type"
        pure to
  If cond cons alt -> do
    condTy <- check cond
    when (condTy /= BoolT) $
      tyErr "condition is not boolean"
    consTy <- check cons
    altTy <- check alt
    when (consTy /= altTy) $
      tyErr "branches of conditional have different types"
    pure consTy
  Unary op e -> do
    t <- check e
    let (UnopTy from to) = unopTy op
    when (from /= t) $
      tyErr "argument of unary operator has the wrong type"
    pure t
  InputInt -> pure Int64T
  Tuple _ -> tyErr "tuples typechecking is not implemented yet"
  where
    check = typecheckExpr env . snd
    tyErr = Left . TypeError expr

data Source ann
  = Expr (Expr ann)
  | Stmt (Stmt ann)
  deriving (Show)

type TC a = Either (TypeError (Source TokenInfo)) a

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = \case
  Left x -> Left (f x)
  Right y -> Right y

typecheckStmt :: TyEnv -> Stmt TokenInfo -> TC TyEnv
typecheckStmt env stmt = case stmt of
  Print e -> do
    t <- check e
    when (t /= Int64T) $
      tyErr "only numeric values can be printed"
    pure env
  Calc e -> do
    _ <- check e
    pure env
  Assign (_, n) e -> do
    t <- check e
    case Map.lookup n env of
      Nothing -> pure (Map.insert n t env)
      Just t2 -> do
        when (t /= t2) $
          tyErr "variable can not change its type!"
        pure env
  IfS cond cons alt -> do
    t <- check cond
    when (t /= BoolT) $
      tyErr "condition expression must be boolean!"
    envCons <- typecheckBlock env cons
    envAlt <- typecheckAlt env alt
    when (envCons /= envAlt) $
      tyErr "both branches of conditional should define same set of variables!"
    pure envCons
  While cond body -> do
    t <- check cond
    when (t /= BoolT) $
      tyErr "condition expression must be boolean!"
    _ <- typecheckBlock env body
    pure env
  where
    check = mapLeft (fmap Expr) . typecheckExpr env . snd
    tyErr = Left . TypeError (Stmt stmt)

typecheckBlock :: TyEnv -> Block TokenInfo -> TC TyEnv
typecheckBlock env (_, ls) = typecheckStatements env ls

typecheckStatements :: TyEnv -> [(TokenInfo, Stmt TokenInfo)] -> TC TyEnv
typecheckStatements env ls = case ls of
  [] -> pure env
  (_, hd) : tl -> do
    env1 <- typecheckStmt env hd
    typecheckStatements env1 tl

typecheckAlt :: TyEnv -> Maybe (Block TokenInfo) -> TC TyEnv
typecheckAlt env alt = case alt of
  Nothing -> pure env
  Just block -> typecheckBlock env block

typecheckModule :: Module -> Maybe (TypeError (Source TokenInfo))
typecheckModule (Module stmts) = case typecheckStatements Map.empty stmts of
  Left err -> Just err
  Right _ -> Nothing

printTypeError :: TypeError (Source TokenInfo) -> Text
printTypeError (TypeError src reason) = Text.unlines
  [ "Error:"
  , "  " <> reason
  , "In:"
  , "  " <> Text.pack (show src)
  ]
