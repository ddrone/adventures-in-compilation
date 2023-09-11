module Grammar.Grammar where

import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Set as Set

import qualified Grammar.Pregrammar as Pregrammar
import Utils (untilEqual)

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
