module CFG.Printer where

import Data.Text (Text)
import qualified Data.Text as Text

import AST (binopRepr, unopRepr)
import CFG.Instr

printName :: GenName -> Text
printName gn = case gn of
  Src s -> s
  Gen s n -> Text.concat [s, "%", Text.pack (show n)]

printBlockName :: BlockName -> Text
printBlockName (BlockName fn n) =
  Text.concat [fn, "$", Text.pack (show n)]

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

indent :: Text -> Text
indent = ("  " <>)

printBlock :: Block -> [Text]
printBlock (Block name asns be) =
  Text.concat [ printBlockName name, ":" ] :
  map indent (map printAssign asns ++ [printBlockEnd be])

printFunction :: Function -> [Text]
printFunction (Function name args body) =
  Text.concat [ "function ", name, "(", Text.intercalate "," args, ") {"] :
  concatMap printBlock body ++ ["}"]
