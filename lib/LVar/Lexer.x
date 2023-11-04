{
module LVar.Lexer where

-- Block comment, /* */
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum = [a-zA-Z0-9]

tokens :-
  $white+ ;
  "#".* ;
  "(" { go }
  ")" { go }
  "+" { go }
  "-" { go }
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
  "+" | "-" | "<=" | "<" | ">=" | ">" | "==" | "!=" | "and" | "or" | "not" { op }
  "=" { go }
  $alpha $alnum* { ident }

{
data Token
  = TokenInt Int
  | TokenLit String
  | TokenIdent String
  | TokenOp String
  | TokenEof
  deriving (Show)

data TokenInfo = TokenInfo
  { tokOffset :: Int
  , tokRow :: Int
  , tokColumn :: Int
  , tokEnd :: Int
  }
  deriving (Show)

action f (AlexPn offset line row) s =
  (TokenInfo offset line row (offset + length s), f s)

go = action TokenLit

ident = action TokenIdent

number = action (TokenInt . read)

op = action TokenOp

data Tokens = Tokens
  { tkTokens :: [(TokenInfo, Token)]
  , tkError :: Maybe TokenInfo
  }
  deriving (Show)

addToken tok (Tokens toks tkErr) = Tokens (tok : toks) tkErr

posToTokenInfo (AlexPn offset line row) = TokenInfo offset line row offset

scanTokens :: String -> Tokens
scanTokens input = go (alexStartPos, '\n', [], input)
  where
    go inp@(pos, _, _, str) =
      case alexScan inp 0 of
        AlexEOF -> Tokens [(posToTokenInfo pos, TokenEof)] Nothing
        AlexSkip inp' len -> go inp'
        AlexToken inp' len act -> addToken (act pos (take len str)) (go inp')
        AlexError (pos, _, _, _) -> Tokens [] (Just (posToTokenInfo pos))
}
