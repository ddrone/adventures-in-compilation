module Main where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text

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

main :: IO ()
main = putStrLn "Here be dragons"
