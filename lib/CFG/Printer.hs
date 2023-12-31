module CFG.Printer where

import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text

import AST (binopRepr, unopRepr, Ident)
import CFG.Instr
import CFG.PhiPlacement (PhiMap, phiPlacement)
import Data.Maybe (fromMaybe)

printName :: GenName -> Text
printName gn = case gn of
  Src s -> s
  Gen s n -> Text.concat [s, "%", Text.pack (show n)]

printAssignSource :: AssignSource -> Text
printAssignSource as = case as of
  Var gn -> printName gn
  Lit l -> Text.pack (show l)
  Call fn args -> Text.concat
    [ fn
    , "("
    , Text.intercalate ", " $ map printName args
    , ")"
    ]
  Bin op l r -> Text.concat
    [ printName l
    , " "
    , binopRepr op
    , " "
    , printName r
    ]
  Unary op e -> Text.concat
    [ unopRepr op
    , printName e
    ]

printAssign :: Assign -> Text
printAssign (Assign tgt src) = Text.concat
  [ printName tgt
  , " = "
  , printAssignSource src
  ]

printBlockEnd :: BlockEnd -> Text
printBlockEnd be = case be of
  Ret n -> Text.concat [ "ret ", printName n ]
  Jump n -> Text.concat [ "jmp ", printBlockName n ]
  CondJump cond cons alt -> Text.concat
    [ "jmp_if "
    , printName cond
    , " "
    , printBlockName cons
    , " "
    , printBlockName alt
    ]

indent :: Text -> Text
indent = ("  " <>)

printBlock :: Block -> [Ident] -> [Text]
printBlock (Block name asns be) phi =
  Text.concat [ printBlockName name, ":" ] :
  Text.concat [ "phi(", Text.intercalate "," phi, ")"] :
  map indent (map printAssign asns ++ [printBlockEnd be])

printFunction :: Function -> [Text]
printFunction fn@(Function name args body startBlock) =
  let phiMap = phiPlacement fn in
  Text.concat [ "function ", name, "(", Text.intercalate "," args, ") {"] :
  Text.concat [ "  start_from ", printBlockName startBlock ] :
  let pb block = printBlock block (fromMaybe [] $ Map.lookup (blockName block) phiMap) in
  concatMap pb body ++ ["}"]
