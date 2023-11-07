module Main where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.List (isSuffixOf)
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.FilePath
import System.Directory
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import LVar.Lexer
import LVar.NewParser hiding (combine)
import LVar.ParseTree

data ParseResponse
  = RespLexerError String TokenInfo
  | RespParserError String TokenInfo
  | RespOK ParseForest
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ParseResponse)

data TestFile = TestFile
  { tfName :: Text
  , tfContents :: Text
  , tfParseResult :: ParseResponse
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''TestFile)

data TestResponse = TestResponse
  { trFiles :: [TestFile]
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''TestResponse)

type API = "parse" :> ReqBody '[PlainText] Text :> Post '[JSON] ParseResponse
  :<|> "test" :> Get '[JSON] TestResponse

api :: Proxy API
api = Proxy

app :: Application
app = serve api (serveSingleParse :<|> serveTests)

serveSingleParse :: Text -> Handler ParseResponse
serveSingleParse input = pure (runParser input)

serveTests :: Handler TestResponse
serveTests = do
  files <- liftIO getTestFiles
  pure (TestResponse files)

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

getTestFiles :: IO [TestFile]
getTestFiles = do
  all <- getDirectoryContents "tests"
  files <- forM (filter (isSuffixOf ".lvar") all) $ \file -> do
    contents <- TextIO.readFile (combine "tests" file)
    pure (TestFile (Text.pack file) contents (runParser contents))
  pure files

main :: IO ()
main = runServer
