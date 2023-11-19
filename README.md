# Adventures in Compilation

Instead of trying to "implement a cool programming language", a project that I have started maybe a dozen times and never got far with, the idea here is to play with different algorithms of compilation on extremely simplistic languages.

## Adventure 9: back to the actual new compilation again, chapter 7 of "Essentials of Compilation"

Took a long break from the book, need to start re-reading the chapter and write down the particular tasks as they appear.

- [x] Add tuples to the parser
- [ ] Add tuple projections to the parser
- [ ] Implement typechecking for tuples

## Adventure 8: reverse mapping

Currently, the compiler works just one way, and it would be close to impossible to figure out where the resulting code comes from. What I want is to have a "source mapping" between each compiler pass, so that it's possible to trace the source of a particular instruction.

- [x] Figure out how to make HLS working again, it's really hard to actually work on Haskell code without any kind of code intelligence features
- [x] Make the UI run the whole compiler and present assembly code
- [x] Test the compiler on having `and` inside while-loop
- [ ] Continue working on this as the demand for debugging the compiler shows up

## Adventure 1: simple SSA construction

- [x] Implement compilation of statements into CFG blocks
- [x] Implement compilation of functions into CFG blocks
- [x] Print out the CFG in some human-readable format
- [x] (Maintenance) Extract Cabal dependencies into a common block to avoid repetition
- [x] Add a new executable "compiler" that will print the CFG at the moment
- [x] Debug the compiler on programs/a01.aic
- [x] Implement interpretation of binary expressions
- [x] Implement compilation of binary expressions
- [x] (Maintenance) Add a warning about non-exhaustive pattern matching to the project
- [x] Implement boolean comparisons in the interpreter
- [x] Add conditionals to the language
- [x] Implement interpretation of conditionals
- [x] Implement compilation of conditionals
- [x] Compute the domination frontier for the CFG
- [ ] Implement conversion of CFG to SSA-form, adding phi-nodes
  - [x] Implement phi-placement algorithm
  - [x] Print the set of variables that have to have phi-nodes for each block in compiler output
  - [ ] Implement the renumbering algorithm
- [ ] Implement conversion out of SSA form
- [ ] Implement code generation to x86-64 assembly
- [ ] Only then add while-loops to the language

## Fun compiler-related sidetracks to do

- Write an interactive typechecker for a functional language
- Try out language workbenches (e.g. MPS) to implement a simple compiler
- Implement my own parser generator

## Bucket list

- Refactor regular expression compilation code: especially subset construction and DFA minimization are quite messy
- Refactor OptimizeBlocks.hs: some code is really messy
- Changing `while e` to `while e and True` introduces extra basic blocks, need to optimize those away

## Useful CLI incantations

Using profiling from Cabal:

```
cabal run --enable-profiling lvar -- tests/cond.lvar +RTS -p
```

Building a specific executable via Stack:

```
stack build :lexemes
```

Running diagnostics on Happy parser:

```
stack exec happy -- lib/LVar/NewParser.y --info=parser_diagnostics.txt --out=/dev/null
```

## Things I've learned while working on this

### Megaparsec expression parsing

Ordering of the parsers in the table for operators matters: if at the same level of precedence I flip parser for `<=` and `<`, parsing of `<=` stops working. Presumably, because the tokens are overlapping. It does not look like there are any diagnostic information available to show me that I'm doing something potentially incorrect (and that's why I actually prefer proper parser generators that have more knowledge about the domain and are able to display those kinds of diagnostics by the way).

### Don't think that typeclass instances do the "obvious thing that I want"

For example, `Monoid` instance for `Data.Map` seems to be just "take the first element", instead of combining the values via monoidal operation (as I've expected).

## Links

* LL(1) grammar visualizer: https://jsmachines.sourceforge.net/machines/ll1.html
* Online Graphviz environment: https://dreampuf.github.io/GraphvizOnline/
* Haskell's parser-combinators documentation: https://hackage.haskell.org/package/parser-combinators
