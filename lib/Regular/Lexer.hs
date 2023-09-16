module Regular.Lexer where

import Regular.DFA
import qualified Data.IntSet as IntSet

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
