# Adventures in Compilation

Instead of trying to "implement a cool programming language", a project that I have started maybe a dozen times and never got far with, the idea here is to play with different algorithms of compilation on extremely simplistic languages.

## Adventure 4: dreaded syntactic analysis

- LL(1)
  - [x] Write down datatypes for context-free grammar
  - [x] Compute nullability information
  - [x] Compute first sets
  - [x] Compute follow sets
  - [ ] Generate LL(1) parse table as an output
  - [ ] Actually test it on a couple of examples
- Regular expressions
  - [ ] Write down datatypes for regular expressions (WITHOUT RANGES)
  - [ ] Implement NFA conversion
  - [ ] Implement NFA-to-DFA conversion
  - [ ] Implement automata minimization
  - [ ] Implement "range" as a primitive for regular language

## Adventure 3: implement LVar from "Essentials of Compilation"

- [x] Add a new executable and package in the code
- [x] Write a simple parser
- [x] Implement the compiler, following the implementation plan from the book
- [x] Write down the dumping of X86 instructions to textual assembly syntax to be able to compile and run programs
- [x] Test on some examples
- [x] Finish the challenge: implement partial evaluator for the language

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

## Adventure 2: graph algorithms for SSA construction

- [x] Finding dominators in directed graph
- [x] Building domination tree
- [x] Write a parser for a simple textual description of graphs
- [x] Write a CLI that will read a graph and spit out in Graphviz with dashed edges for parent relation in domination tree
- [x] Debug the dominator tree algorithm
- [x] Computing domination frontier
- [x] Write a CLI to print out domination frontier for a graph

## Things I've learned while working on this

### Megaparsec expression parsing

Ordering of the parsers in the table for operators matters: if at the same level of precedence I flip parser for `<=` and `<`, parsing of `<=` stops working. Presumably, because the tokens are overlapping. It does not look like there are any diagnostic information available to show me that I'm doing something potentially incorrect (and that's why I actually prefer proper parser generators that have more knowledge about the domain and are able to display those kinds of diagnostics by the way).
