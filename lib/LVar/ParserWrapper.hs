module LVar.ParserWrapper where

import Data.Text (Text)

import LVar.Lexer
import LVar.NewParser

runParser :: Tokens -> Either (TokenInfo, String) [S TokenInfo]
runParser tokens =
  case runP parse (tkTokens tokens) of
    Left (info, msg) -> Left (info, msg)
    Right ((_, r), _) -> Right r
