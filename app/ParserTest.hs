module Main where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import LVar.Lexer
import LVar.NewParser
import LVar.ParseTree

data ParseResponse
  = RespLexerError String TokenInfo
  | RespParserError String TokenInfo
  | RespOK ParseForest
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ParseResponse)

type API = "parse" :> ReqBody '[PlainText] Text :> Post '[JSON] ParseResponse

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Text -> Handler ParseResponse
server input = pure (runParser input)

runServer :: IO ()
runServer = run 8080 app

runParser :: Text -> ParseResponse
runParser input =
  let tokens = scanTokens input in
  case tkError tokens of
    Just ti -> RespLexerError "Lexer error" ti
    Nothing ->
      case runP parse (tkTokens tokens) of
        Left (info, msg) -> RespParserError msg info
        Right ((_, r), _) -> RespOK (toParseForest r)

main :: IO ()
main = runServer
