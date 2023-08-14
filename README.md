# Adventures in Compilation

Instead of trying to "implement a cool programming language", a project that I have started maybe a dozen times and never got far with, the idea here is to play with different algorithms of compilation on extremely simplistic languages.

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
- [ ] Add conditionals to the language
- [ ] Implement conversion of CFG to SSA-form, adding phi-nodes
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
