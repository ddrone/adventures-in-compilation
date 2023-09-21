module LVar.PartialEval where
import LVar.AST (Binop(..), Unop(..))
import Data.Int (Int64)

data Value
  = Int64 Int64
  | Bool Bool
  deriving (Show, Eq)

raiseInt64Binop :: (Int64 -> Int64 -> Int64) -> Value -> Value -> Maybe Value
raiseInt64Binop b x y = case (x, y) of
  (Int64 x1, Int64 y1) -> Just (Int64 (b x1 y1))
  _ -> Nothing

raiseInt64Cmp :: (Int64 -> Int64 -> Bool) -> Value -> Value -> Maybe Value
raiseInt64Cmp b x y = case (x, y) of
  (Int64 x1, Int64 y1) -> Just (Bool (b x1 y1))
  _ -> Nothing

raiseBoolBinop :: (Bool -> Bool -> Bool) -> Value -> Value -> Maybe Value
raiseBoolBinop b x y = case (x, y) of
  (Bool x1, Bool y1) -> Just (Bool (b x1 y1))
  _ -> Nothing

eqBinop :: Value -> Value -> Maybe Value
eqBinop x y = case (x, y) of
  (Int64 x1, Int64 y1) -> Just (Bool (x1 == y1))
  (Bool x1, Bool y1) -> Just (Bool (x1 == y1))
  _ -> Nothing

neqBinop :: Value -> Value -> Maybe Value
neqBinop x y = case (x, y) of
  (Int64 x1, Int64 y1) -> Just (Bool (x1 /= y1))
  (Bool x1, Bool y1) -> Just (Bool (x1 /= y1))
  _ -> Nothing

evalBinop :: Binop -> Value -> Value -> Maybe Value
evalBinop = \case
  Add -> raiseInt64Binop (+)
  Sub -> raiseInt64Binop (-)
  Le -> raiseInt64Cmp (<=)
  Lt -> raiseInt64Cmp (<)
  Ge -> raiseInt64Cmp (>=)
  Gt -> raiseInt64Cmp (>)
  Eq -> eqBinop
  Ne -> neqBinop
  And -> raiseBoolBinop (&&)
  Or -> raiseBoolBinop (||)

evalUnop :: Unop -> Value -> Maybe Value
evalUnop uo v = case (uo, v) of
  (Neg, Int64 x) -> Just (Int64 (-x))
  (Not, Bool x) -> Just (Bool (not x))
  _ -> Nothing
