module Main where

import Regular.Expr
import Regular.NFA
import Regular.DFA
import Regular.Lexer
import qualified Data.Text.IO as TextIO

digit = range '0' '9'
number = plus digit
numbers = star (number `cat` char ',') `cat` number
list = char '[' `cat` numbers `cat` char ']'

nfa = buildNFA list
dfa1 = buildDFA nfa
dfa2 = minimizeDFA dfa1

testInput = "[12,23][45,67][89]"

main = do
  -- TextIO.putStr (printNFA nfa)
  TextIO.putStr (printDFA dfa2)
  case splitTokens dfa2 testInput of
    Nothing -> putStrLn "splitting failed!"
    Just tokens -> mapM_ putStrLn tokens
