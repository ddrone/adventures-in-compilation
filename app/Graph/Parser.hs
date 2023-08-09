module Graph.Parser where

import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as Text

import Parser (Parser)
import Graph.Defs (Graph, fromEdges)
import qualified Parser

nodeName :: Parser Text
nodeName = Parser.lexeme $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_' <|> char '-')
  pure (Text.pack (first : rest))

data NodeDef = NodeDef
  { ndName :: Text
  , ndSuccessors :: [Text]
  }
  deriving (Show)

nodeDef :: Parser NodeDef
nodeDef = do
  from <- nodeName
  Parser.symbol "->"
  to <- many nodeName
  Parser.symbol ";"
  pure (NodeDef from to)
