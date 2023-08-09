module Graph.Parser where

import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Tuple (swap)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

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

data ParsedGraph = ParsedGraph
  { pgNames :: IntMap Text
  , pgGraph :: Graph
  }

buildGraph :: [NodeDef] -> ParsedGraph
buildGraph nodeDefs = do
  let addNode (NodeDef from to) nodeSet = foldr Set.insert nodeSet (from : to)
  let allNodes = zip (enumFrom (0 :: Int)) . Set.toList $ foldr addNode Set.empty nodeDefs
  let nameToId = Map.fromList (swap <$> allNodes)
  let getId v = fromJust (Map.lookup v nameToId)
  let nodeToEdges (NodeDef from to) = map ((,) (getId from) . getId) to
  let graph = fromEdges (concatMap nodeToEdges nodeDefs)
  ParsedGraph (IntMap.fromList allNodes) graph
