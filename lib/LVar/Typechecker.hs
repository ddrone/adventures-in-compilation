module LVar.Typechecker where
import LVar.AST (Expr (..), Binop (..), Unop (..))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when)

data Type
  = Int64T
  | BoolT
  deriving (Show, Eq, Ord)

data TypeError = TypeError
  { teExpr :: Expr
  , teReason :: Text
  }
  deriving (Show)

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

typecheckExpr :: Map Text Type -> Expr -> Either TypeError Type
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
  where
    check = typecheckExpr env
    tyErr = Left . TypeError expr
