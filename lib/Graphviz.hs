module Graphviz where

import Data.Text (Text)
import qualified Data.Text as Text

data Attr = Attr
  { attrName :: Text
  , attrValue :: Text
  }
  deriving (Show)

data Node = Node
  { nodeName :: Text
  , nodeAttrs :: [Attr]
  }
  deriving (Show)

data Edge = Edge
  { edgeFrom :: Text
  , edgeTo :: Text
  , edgeAttrs :: [Attr]
  }
  deriving (Show)

attr :: Text -> Text -> Attr
attr = Attr

node :: Text -> Node
node name = Node name []

nodeA :: Text -> [Attr] -> Node
nodeA = Node

edge :: Text -> Text -> Edge
edge from to = Edge from to []

edgeA :: Text -> Text -> [Attr] -> Edge
edgeA = Edge

printAttr :: Attr -> Text
printAttr (Attr name value) = Text.concat [name, "=", value]

printAttrs :: [Attr] -> Text
printAttrs ls = case ls of
  [] -> ""
  _ -> Text.concat ["[", Text.intercalate "," (map printAttr ls), "]"]

printNode :: Node -> Text
printNode (Node name attrs) = Text.concat ["  ", name, printAttrs attrs, ";"]

printEdge :: Edge -> Text
printEdge (Edge from to attrs) = Text.concat ["  ", from, " -> ", to, printAttrs attrs, ";"]

printGraphN :: Text -> [Node] -> [Edge] -> Text
printGraphN name nodes edges =
  let prefix = Text.concat ["digraph ", name, " {"]
      suffix = "}"
      lines = prefix : map printNode nodes ++ map printEdge edges ++ [suffix]
  in Text.unlines lines

printGraph :: [Node] -> [Edge] -> Text
printGraph = printGraphN "G"
