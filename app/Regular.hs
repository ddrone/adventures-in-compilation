module Main where

import Data.Map (Map)
import Regular.Expr
import Regular.NFA
import Regular.DFA
import Regular.Lexer
import qualified Data.Map as Map
import qualified Data.Text.IO as TextIO
import Control.Monad (forM_)

digit = range '0' '9'
number = plus digit
numbers = star (number `cat` char ',') `cat` number
list = char '[' `cat` optional numbers `cat` char ']'

testInput = "1238[12,23]123[45,67]3[89][][]abc"

spec = Map.fromList [(0, number), (1, list)]

nfa = buildLabeledNFA spec
dfa1 = buildLDFA nfa
dfa2 = minimizeLabelledDFA dfa1


main = do
  -- TextIO.putStr (printNFA nfa)
  -- TextIO.putStr (printDFA dfa2)
  let (Tokens ts rest) = splitLabeledTokens dfa2 testInput
  forM_ ts $ \(classId, token) -> do
    putStr "token "
    putStr (show classId)
    putStr ", "
    putStrLn token
  case rest of
    [] -> pure ()
    _ -> putStr "unparsed input: " >> putStrLn rest
