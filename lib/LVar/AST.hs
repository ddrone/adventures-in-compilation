module LVar.AST
  ( module LVar.AST
  , module LVar.Operators
  , Expr(..)
  , Stmt(..)
  ) where

import Control.Arrow ((***), second)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Set as Set

import LVar.Operators
import LVar.Lexer (TokenInfo)
import LVar.NewParser (Expr(..), E, Stmt(..))

comparisonOps :: Set Binop
comparisonOps = Set.fromList [Le, Lt, Ge, Gt, Eq, Ne]

isComparisonOp :: Binop -> Bool
isComparisonOp = flip Set.member comparisonOps

binopRepr :: Binop -> Text
binopRepr = \case
  Add -> "+"
  Sub -> "-"
  Le -> "<="
  Lt -> "<"
  Ge -> ">="
  Gt -> ">"
  Eq -> "=="
  Ne -> "!="
  And -> "and"
  Or -> "or"

prependUnop :: Text -> Unop -> Text
prependUnop t = \case
  Neg -> "-" <> t
  Not -> "not " <> t

unopRepr :: Unop -> Text
unopRepr = \case
  Neg -> "-"
  Not -> "not"

exprPlate :: (Expr a -> Expr a) -> Expr a -> Expr a
exprPlate c e = case e of
  -- Apply the function to constructors that actually have children
  Bin op (i1, e1) (i2, e2) -> Bin op (i1, c e1) (i2, c e2)
  If (iCond, cond) (iCons, cons) (iAlt, alt) ->
    If (iCond, c cond) (iCons, c cons) (iAlt, c alt)
  Unary op (i, e) -> Unary op (i, c e)
  -- Don't do anything to the constructors that do not have children
  Const _ -> e
  Bool _ -> e
  Name _ -> e
  InputInt -> e
  Tuple es -> Tuple (map (second c) es)
  Proj tup i -> Proj (second c tup) (second c i)

mapInfoE :: (Expr a -> Expr a) -> E a -> E a
mapInfoE f (i, x) = (i, f x)

exprTopdown :: (Expr a -> Expr a) -> Expr a -> Expr a
exprTopdown transform = transform . exprPlate (exprTopdown transform)

mapExpr :: (Expr a -> Expr a) -> Stmt a -> Stmt a
mapExpr f = \case
  Print (i, e) -> Print (i, f e)
  Calc (i, e) -> Calc (i, f e)
  Assign n (i, e) -> Assign n (i, f e)
  IfS (iCond, cond) (iCons, cons) alt ->
    IfS (iCond, f cond) (iCons, map (second (mapExpr f)) cons) mappedAlt
    where
      mappedAlt = case alt of
        Nothing -> Nothing
        Just (iAlt, block) -> Just (iAlt, map (second (mapExpr f)) block)
  While (ie, e) (info, body) -> While (ie, f e) (info, map (second (mapExpr f)) body)

newtype GenModule s ann = Module
  { modStmts :: [(ann, s)]
  }
  deriving (Show)

type Module = GenModule (Stmt TokenInfo) TokenInfo

stripAnn :: Module -> GenModule (Stmt ()) ()
stripAnn (Module ss) = Module (map (const () *** fmap (const ())) ss)

mapModule :: (Expr ann -> Expr ann) -> GenModule (Stmt ann) ann -> GenModule (Stmt ann) ann
mapModule f (Module stmts) = Module (map (second (mapExpr f)) stmts)
