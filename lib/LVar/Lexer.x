{
module LVar.Lexer
  ( module LVar.LexerDeps
  , module LVar.Lexer
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import LVar.LexerDeps

-- TODO: Block comment, /* */
}

%wrapper "posn-strict-text"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum = [a-zA-Z0-9]

tokens :-
  $white+ ;
  "#".* ;
  "(" { go }
  ")" { go }
  $digit+ { number }
  "True" { go }
  "False" { go }
  "input_int" { go }
  "if" { go }
  "else" { go }
  "{" { go }
  "}" { go }
  "print" { go }
  "while" { go }
  "tuple" { go }
  ";" { go }
  "," { go }
  "+" | "-" | "<=" | "<" | ">=" | ">" | "==" | "!=" | "and" | "or" | "not" { op }
  "=" { go }
  $alpha $alnum* { ident }

{
data Token
  = TokenInt Int64
  | TokenLit Text
  | TokenIdent Text
  | TokenOp Text
  | TokenEof
  deriving (Show)

action f (AlexPn offset line row) s =
  (TokenInfo offset line row (offset + Data.Text.length s), f s)

go = action TokenLit

ident = action TokenIdent

number = action (TokenInt . read . Data.Text.unpack)

op = action TokenOp

data Tokens = Tokens
  { tkTokens :: [(TokenInfo, Token)]
  , tkError :: Maybe TokenInfo
  }
  deriving (Show)

addToken tok (Tokens toks tkErr) = Tokens (tok : toks) tkErr

posToTokenInfo (AlexPn offset line row) = TokenInfo offset line row offset

scanTokens :: Text -> Tokens
scanTokens input = go (alexStartPos, '\n', [], input)
  where
    go inp@(pos, _, _, str) =
      case alexScan inp 0 of
        AlexEOF -> Tokens [(posToTokenInfo pos, TokenEof)] Nothing
        AlexSkip inp' len -> go inp'
        AlexToken inp' len act -> addToken (act pos (Data.Text.take len str)) (go inp')
        AlexError (pos, _, _, _) -> Tokens [] (Just (posToTokenInfo pos))
}
