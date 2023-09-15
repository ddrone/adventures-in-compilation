module Main where

import Regular.Expr

digit = range '0' '9'
number = plus digit
numbers = star (number `cat` char ',') `cat` number
list = char '[' `cat` numbers `cat` char ']'

nfa = buildNFA (char 'o' `cat` char 'a')
dfa = buildDFA nfa

main = do
  print nfa
  print dfa