module Parser where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

import AST

type Parser = Parsec Void Text

program :: Parser Program
program = pure []
