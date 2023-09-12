module Grammar.Grammar where

import Data.Maybe (fromMaybe)
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

isRuleNullable :: Set Text -> Rule -> Bool
isRuleNullable nulls (Rule _ items) = all (isItemNullable nulls) items

nullable :: Grammar -> Set Text
nullable rules = untilEqual iter Set.empty
  where
    iter nulls =
      let nullRules = filter (isRuleNullable nulls) rules in
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
