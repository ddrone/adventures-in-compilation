module Main where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import LVar.Lexer
import LVar.NewParser

data ParseResponse
  = RespLexerError String TokenInfo
  | RespParserError String TokenInfo
  | RespOK
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ParseResponse)

type API = "parse" :> ReqBody '[PlainText] String :> Post '[JSON] ParseResponse

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: String -> Handler ParseResponse
server input = pure RespOK

runServer :: IO ()
runServer = run 8080 app

main :: IO ()
main = do
  s <- getContents
  let lexemes = scanTokens s
  print (runP parse (tkTokens lexemes))

