module Regular.Lexer where

import Regular.DFA
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap

splitTokens :: DFA -> String -> Maybe [String]
splitTokens dfa input = splitTokensIter dfa (dfaStart dfa) Nothing [] input

splitTokensIter :: DFA -> Int -> Maybe (String, String) -> String -> String -> Maybe [String]
splitTokensIter dfa state lastMatch acc input =
  case input of
    [] -> case acc of
      [] -> Just []
      _ -> useState
    c : cs -> case next dfa state c of
      Nothing -> useState
      Just nextState ->
        let newLastMatch =
              if IntSet.member nextState (dfaFinal dfa)
                then Just (reverse (c : acc), cs)
                else lastMatch
        in splitTokensIter dfa nextState newLastMatch (c : acc) cs
  where
    useState = case lastMatch of
      Nothing -> Nothing
      Just (tok, rest) -> (tok :) <$> splitTokensIter dfa (dfaStart dfa) Nothing [] rest

data Tokens l = Tokens
  { tokensActual :: [(l, String)]
  , tokensRest :: String
  }
  deriving (Show)

errorTokens :: String -> Tokens l
errorTokens rest = Tokens [] rest

consToken :: (l, String) -> Tokens l -> Tokens l
consToken hd (Tokens tl rest) = Tokens (hd : tl) rest

emptyTokens :: Tokens l
emptyTokens = Tokens [] []

splitLabeledTokens :: LabeledDFA l -> String -> Tokens l
splitLabeledTokens dfa input = splitLabeledTokensIter dfa (ldfaStart dfa) Nothing [] input

data SavedState l = SavedState
  { ssToken :: String
  , ssClass :: l
  , ssRest :: String
  }

splitLabeledTokensIter :: LabeledDFA l -> Int -> Maybe (SavedState l) -> String -> String -> Tokens l
splitLabeledTokensIter dfa state lastMatch acc input =
  case input of
    [] -> case acc of
      [] -> emptyTokens
      _ -> useState
    c : cs -> case nextL dfa state c of
      Nothing -> useState
      Just nextState ->
        let newLastMatch =
              case IntMap.lookup nextState (ldfaFinal dfa) of
                Just classId -> Just (SavedState (reverse (c : acc)) classId cs)
                Nothing -> Nothing
        in splitLabeledTokensIter dfa nextState newLastMatch (c : acc) cs
  where
    useState = case lastMatch of
      Nothing -> errorTokens (reverse acc ++ input)
      Just (SavedState token classId rest) -> consToken (classId, token) (splitLabeledTokensIter dfa (ldfaStart dfa) Nothing [] rest)
