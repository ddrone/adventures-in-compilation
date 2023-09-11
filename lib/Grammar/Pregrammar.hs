module Grammar.Pregrammar where

import Data.Text (Text)

data Rule = Rule
  { ruleStart :: Text
  , ruleItems :: [Text]
  }
  deriving (Show)

type Grammar = [Rule]
