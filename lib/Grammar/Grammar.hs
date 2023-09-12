module Grammar.Grammar where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Utils (untilEqual)
import qualified Grammar.Pregrammar as Pregrammar

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
  | N Text -- NT is taken
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
          let add1 = Set.map N (ruleFirst nulls firsts rest)
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

instance Semigroup PreTableRow where
  PreTableRow n1 <> PreTableRow n2 = PreTableRow (n1 <> n2)

instance Monoid PreTableRow where
  mempty = PreTableRow Map.empty

type PreTable = Map Text PreTableRow

buildPreTable :: Grammar -> Text -> PreTable
buildPreTable grammar start =
  let
    nulls = nullable grammar
    firsts = first grammar nulls
    follows = follow grammar start nulls firsts

    handleRule (Rule from items) =
      let rfirsts = Set.toList (ruleFirst nulls firsts items)
          rmfirsts =
            Map.fromList (flip map rfirsts (\x -> (N x, [items])))
          rfollows = maybe [] Set.toList (Map.lookup from follows)
          rnull = isRuleNullable nulls items
          reof =
            if rnull
              then Map.fromList (flip map rfollows (\x -> (x, [items])))
              else Map.empty
      in Map.singleton from (PreTableRow (rmfirsts <> reof))
  in mconcat (map handleRule grammar)

preTableRowToRow :: PreTableRow -> Maybe LLTableRow
preTableRowToRow (PreTableRow m) =
  let items = Map.toList m
      eofItem = Map.lookup EOF m
      getNtItem (item, v) = case item of
        N n -> Just (n, v)
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
