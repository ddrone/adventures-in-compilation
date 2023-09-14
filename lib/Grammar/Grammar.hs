module Grammar.Grammar where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Utils (untilEqual, printTable)
import qualified Grammar.Pregrammar as Pregrammar
import qualified Data.Text as Text

data ItemType
  = NT -- non-terminal
  | T  -- terminal
  deriving (Show, Enum)

data Item = Item
  { itemType :: ItemType
  , itemName :: Text
  }
  deriving (Show)

data Rule = Rule
  { ruleStart :: Text
  , ruleItems :: [Item]
  }
  deriving (Show)

type Grammar = [Rule]

addTerminal :: Item -> Set Text -> Set Text
addTerminal item ts = case item of
  Item T n -> Set.insert n ts
  _ -> ts

addRuleTerminals :: Rule -> Set Text -> Set Text
addRuleTerminals (Rule _ items) ts = foldr addTerminal ts items

grammarTerminals :: Grammar -> Set Text
grammarTerminals rules = foldr addRuleTerminals Set.empty rules

fromPregrammarItem :: Set Text -> Text -> Item
fromPregrammarItem nts item =
  if Set.member item nts
    then Item NT item
    else Item T item

fromPregrammarRule :: Set Text -> Pregrammar.Rule -> Rule
fromPregrammarRule nts (Pregrammar.Rule start items) =
  Rule start (map (fromPregrammarItem nts) items)

fromPregrammar :: Pregrammar.Grammar -> Grammar
fromPregrammar pg =
  let nts = Set.fromList (map Pregrammar.ruleStart pg) in
  map (fromPregrammarRule nts) pg

isItemNullable :: Set Text -> Item -> Bool
isItemNullable nulls (Item t n) =
  case t of
    T -> False
    NT -> Set.member n nulls

isRuleNullable :: Set Text -> [Item] -> Bool
isRuleNullable nulls items = all (isItemNullable nulls) items

nullable :: Grammar -> Set Text
nullable rules = untilEqual iter Set.empty
  where
    iter nulls =
      let nullRules = filter (isRuleNullable nulls . ruleItems) rules in
      nulls `Set.union` (Set.fromList (map ruleStart nullRules))

type FirstMap = Map Text (Set Text)

ruleFirst :: Set Text -> FirstMap -> [Item] -> Set Text
ruleFirst nulls firsts items = case items of
  [] -> Set.empty
  Item T name : rest -> Set.singleton name
  Item NT name : rest ->
    let hd = fromMaybe Set.empty (Map.lookup name firsts) in
    if Set.member name nulls
      then hd <> ruleFirst nulls firsts rest
      else hd

rulesFirst :: Set Text -> FirstMap -> [Rule] -> FirstMap
rulesFirst nulls firsts rules = case rules of
  [] -> firsts
  Rule st items : rest ->
    let itemsFirsts = ruleFirst nulls firsts items
        nextMap = Map.insertWith Set.union st itemsFirsts firsts
    in rulesFirst nulls nextMap rest

first :: Grammar -> Set Text -> FirstMap
first rules nulls = untilEqual iter Map.empty
  where
    iter map = rulesFirst nulls map rules

data FollowItem
  = EOF
  | Tm Text -- T is taken
  deriving (Eq, Show, Ord)

type FollowMap = Map Text (Set FollowItem)

follow :: Grammar -> Text -> Set Text -> FirstMap -> FollowMap
follow rules start nulls firsts = untilEqual iter (Map.singleton start (Set.singleton EOF))
  where
    followRule :: FollowMap -> Text -> [Item] -> FollowMap
    followRule map start rule =
      case rule of
        [] -> map
        Item NT n : rest ->
          let add1 = Set.map Tm (ruleFirst nulls firsts rest)
              add2 =
                if isRuleNullable nulls rest
                  then fromMaybe Set.empty (Map.lookup start map)
                  else Set.empty
          in followRule (Map.insertWith Set.union n (add1 <> add2) map) start rest
        _ : rest -> followRule map start rest
    followRules grammar map =
      case grammar of
        [] -> map
        Rule start items : rest -> followRules rest (followRule map start items)
    iter = followRules rules

data LLTableRow = LLTableRow
  { llNext :: Map Text [Item] -- mapping from lookahead terminal to next rule
  , llEofNext :: Maybe [Item] -- expansion if end-of-input encountered
  }
  deriving (Show)

type LLTable = Map Text LLTableRow -- mapping from non-terminal to table row

data PreTableRow = PreTableRow
  { ptNext :: Map FollowItem [[Item]]
  }
  deriving (Show)

type PreTable = Map Text PreTableRow

data AnalyzedGrammar = AnalyzedGrammar
  { agRules :: Grammar
  , agStart :: Text
  , agNullable :: Set Text
  , agFirst :: FirstMap
  , agFollow :: FollowMap
  }
  deriving (Show)

analyzeGrammar :: Grammar -> Text -> AnalyzedGrammar
analyzeGrammar grammar start =
  let
    nulls = nullable grammar
    firsts = first grammar nulls
    follows = follow grammar start nulls firsts
  in AnalyzedGrammar grammar start nulls firsts follows

combineMaps :: Ord k => Map k [a] -> Map k [a] -> Map k [a]
combineMaps = Map.unionWith (++)

combineRows :: PreTableRow -> PreTableRow -> PreTableRow
combineRows (PreTableRow r1) (PreTableRow r2) = PreTableRow (combineMaps r1 r2)

buildPreTable :: AnalyzedGrammar -> PreTable
buildPreTable grammar =
  let
    nulls = agNullable grammar
    firsts = agFirst grammar
    follows = agFollow grammar

    handleRule (Rule from items) =
      let rfirsts = Set.toList (ruleFirst nulls firsts items)
          rmfirsts =
            Map.fromList (flip map rfirsts (\x -> (Tm x, [items])))
          rfollows = maybe [] Set.toList (Map.lookup from follows)
          rnull = isRuleNullable nulls items
          reof =
            if rnull
              then Map.fromList (flip map rfollows (\x -> (x, [items])))
              else Map.empty
      in Map.singleton from (PreTableRow (combineMaps rmfirsts reof))
  in foldr (Map.unionWith combineRows) Map.empty (map handleRule (agRules grammar))

preTableRowToRow :: PreTableRow -> Maybe LLTableRow
preTableRowToRow (PreTableRow m) =
  let items = Map.toList m
      eofItem = Map.lookup EOF m
      getNtItem (item, v) = case item of
        Tm n -> Just (n, v)
        _ -> Nothing
      ntItems = catMaybes (map getNtItem items)
      pickOne ls = case ls of
        [] -> Just Nothing
        [x] -> Just (Just x)
        _ -> Nothing
      pickNts nts =
        case nts of
          [] -> Just []
          (n, ls) : rest -> do
            one <- pickOne ls
            rests <- pickNts rest
            case one of
              Nothing -> pure rests
              Just r -> pure ((n, r) : rests)

      actualEof = case eofItem of
        Nothing -> Just Nothing
        Just ls -> pickOne ls
      actualNtItems = Map.fromList <$> pickNts ntItems
  in LLTableRow <$> actualNtItems <*> actualEof

preTableToTable :: PreTable -> Maybe LLTable
preTableToTable = traverse preTableRowToRow

ll1Table :: AnalyzedGrammar -> Maybe LLTable
ll1Table = preTableToTable . buildPreTable

printRule :: [Item] -> Text
printRule items = case items of
  [] -> "ε"
  ls -> Text.unwords (map itemName ls)

printSet :: [Text] -> Text
printSet items = case items of
  [] -> "∅"
  ls -> Text.intercalate ", " items

printFollowItem :: FollowItem -> Text
printFollowItem item = case item of
  EOF -> "EOF"
  Tm n -> n

printTableRow :: AnalyzedGrammar -> [Text] -> Text -> LLTableRow -> [Text]
printTableRow grammar terminals start row =
  start : map forTerminal terminals ++ [renderLookup (llEofNext row), null, firsts, follow]
  where
    renderLookup = maybe "" printRule
    forTerminal t = renderLookup (Map.lookup t (llNext row))
    null = if Set.member start (agNullable grammar) then "x" else ""
    doLookup m = Set.toList (fromMaybe Set.empty (Map.lookup start m))
    firsts = Text.unwords (doLookup (agFirst grammar))
    follow = Text.unwords (map printFollowItem (doLookup (agFollow grammar)))

printLL1Table :: AnalyzedGrammar -> Maybe Text
printLL1Table grammar =
  let terminals = Set.toList (grammarTerminals (agRules grammar))
      header = "Symbol" : terminals ++ ["EOF", "Nullable", "First", "Follow"]
      table = ll1Table grammar
  in
    case table of
      Nothing -> Nothing
      Just t -> Just (printTable header (map (uncurry (printTableRow grammar terminals)) (Map.toList t)))

printPreTableRow :: AnalyzedGrammar -> [Text] -> Text -> PreTableRow -> [Text]
printPreTableRow grammar terminals start row =
  start : map forTerminal terminals ++ [ forItem EOF ]
  where
    forItem i =
      let rules = fromMaybe [] (Map.lookup i (ptNext row))
      in Text.intercalate ", " (map printRule rules)

    forTerminal t = forItem (Tm t)

printPreTable :: AnalyzedGrammar -> Text
printPreTable grammar =
  let terminals = Set.toList (grammarTerminals (agRules grammar))
      header = "Symbol" : terminals ++ ["EOF"]
      table = buildPreTable grammar
  in printTable header (map (uncurry (printPreTableRow grammar terminals)) (Map.toList table))

printGrammarRule :: AnalyzedGrammar -> Rule -> [Text]
printGrammarRule ag (Rule start items) =
  let firsts = Set.toList (ruleFirst (agNullable ag) (agFirst ag) items)
      printedFirsts = printSet firsts
  in
    [Text.unwords [start, "::=", printRule items], printedFirsts]

printGrammar :: AnalyzedGrammar -> Text
printGrammar ag = printTable ["Rule", "First"] (map (printGrammarRule ag) (agRules ag))
