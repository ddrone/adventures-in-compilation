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

data GraphType
  = DigraphT
  | GraphT
  deriving (Show)

data Graph = Graph
  { graphType :: GraphType
  , graphName :: Text
  , graphNodes :: [Node]
  , graphEdges :: [Edge]
  }

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
printAttr (Attr name value) = Text.concat [name, "=\"", value, "\""]

printAttrs :: [Attr] -> Text
printAttrs ls = case ls of
  [] -> ""
  _ -> Text.concat [" [", Text.intercalate "," (map printAttr ls), "]"]

printNode :: Node -> Text
printNode (Node name attrs) = Text.concat ["  ", name, printAttrs attrs, ";"]

printEdge :: GraphType -> Edge -> Text
printEdge ty (Edge from to attrs) =
  let edge = case ty of
        DigraphT -> " -> "
        GraphT -> " -- "
  in Text.concat ["  ", from, edge, to, printAttrs attrs]

printGraphType :: GraphType -> Text
printGraphType ty = case ty of
  DigraphT -> "digraph "
  GraphT -> "graph "

printGraph :: Graph -> Text
printGraph (Graph ty name nodes edges) =
  let prefix = Text.concat [printGraphType ty, name, " {"]
      suffix = "}"
      lines = prefix : map printNode nodes ++ map (printEdge ty) edges ++ [suffix]
  in Text.unlines lines

printDigraph :: [Node] -> [Edge] -> Text
printDigraph nodes edges = printGraph (Graph DigraphT "G" nodes edges)

printUndirectedGraph :: [Node] -> [Edge] -> Text
printUndirectedGraph nodes edges = printGraph (Graph GraphT "G" nodes edges)
