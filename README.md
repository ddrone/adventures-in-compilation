# Adventures in Compilation

Instead of trying to "implement a cool programming language", a project that I have started maybe a dozen times and never got far with, the idea here is to play with different algorithms of compilation on extremely simplistic languages.

## Adventure 6: convert LVar to LIf following chapter 5 of Essentials of Compilation

- [x] Implement postfix conditional in the parser
- [x] Add new operators and fix up the partial evaluator
- [x] Figure out why partial evaluation seems to be getting stuck on new test (turned out to be RCO pass, which gets in an infinite loop due to wrong assumptions)
- [x] Add conditional statements to the AST and parse them
- [x] Improve printing of parse errors
- [x] Fix the issue with parsing of conditional statements
- [x] Write type checker for the source language
- [x] Connect the type checker to the compiler
- [x] Implement shrinking pass that gets rid of `and` and `or` binary operators
- [x] Extend RCO pass to the new language features, adding necessary constructors to the IR
- [x] Write AST definitions for new C-like intermediate language
- [x] Implement "explicate control" pass
- [x] Implement human-readable printing of C-like IR
- [x] Implement human-readable printing of monadic IR as well
- [x] Check that explicate control works similarly to the book (it's not, but it's close enough. Not sure how to make it match though)
- [x] Add new X86 constructs to its abstract syntax
- [x] Implement printing those new X86 constructs into concrete syntax used by GCC
- [x] Create a module for directed graphs
- [x] Convert C-like IR module to a directed graph of jumps needed for liveness analysis
- [x] Implement topological sorting for arbitrary graphs
- [x] Print the topological sorting of blocks in C-like IR
- [x] Implement liveness analysis for acyclic CFG
- [x] Change the computation of interference graph to use the full liveness information
- [x] Write the code to perform location replacement on graph of blocks
- [x] Change the output assembly generation to include labels as well
- [x] Test the changes to see if the compiled code actually works
- [x] Change the code so that conclusion is generated with additional label
- [x] Make the partial evaluation work for conditional expressions
- [x] Finish peCmp function
- [x] Remove CmpLit constructor
- [x] Figure out why one of the tests does not compile successfully (some locations do not show up in the interference graph)
- [x] Write a function to print X86 code with names in them, together with liveness information.
- [x] There seems to be a mess in the X86-with-variables output that immediates are printed in the same way as variables. Fix it.
- [ ] Collect all locations present in the program to compute the coloring instead of relying on those present in interference graph
- [ ] Remove `traceShow` from Compiler.hs
- [ ] Refactor patchInstruction to reduce the amount of boilerplate, also add support for `xorq` and maybe `cmpq` instrctions (find out if that's really necessary first)

## Adventure 5: back to actual compilation; implement register allocation for LVar

- [x] Open the book, find the place where I've stopped and write down the actual task list
- [x] Restart reading text from chapter 4 and find the code that I've already implemented for it
- [x] Implement building an interference graph
- [x] Convert interference graph into Graphviz format
- [x] Test building an interference graph: compute it for the sequence of instructions given in the book
- [x] Change the interference graph building such that only variables are considered
- [x] Read section 4.4 and implement graph coloring
- [x] Test saturation coloring on the example from the book
- [x] Write a function that checks that the coloring of nodes is valid
- [x] Add registers back to the interference graph
- [x] Assign the register indices to nodes corresponding to the registers in the initial coloring
- [x] Preserve callee-saved registers on the stack
- [x] Write a script to recompile all the test cases and commit changes to assembly code, to more easily see how the changes in the compiler affect the resulting code
- [x] Clean up the debug output of LVar compiler
- [x] Compute graph of move-related locations
- [x] Make the heuristic for choosing the next vertex to be colored a parameter of graph coloring algorithm
- [x] Implement the heuristic that uses the graph of move-related variables
- [x] See if it's actually going to change anything on the examples that I already have

## Adventure 4: dreaded syntactic analysis

- LL(1)
  - [x] Write down datatypes for context-free grammar
  - [x] Compute nullability information
  - [x] Compute first sets
  - [x] Compute follow sets
  - [x] Generate LL(1) parse table as an output
  - [x] Extract set of all terminals from the grammar
  - [x] Write a generic function for printing a table
  - [x] Print LL(1) parse table in a better way
  - [x] Print first and follow sets in a table as well to aid debugging
  - [x] Print pretable as well
  - [x] Write a function to print all the rules of the grammar together with possible beginnings
  - [x] Change printing of a grammar to use table as well
  - [x] Actually test it on a couple of examples
- Regular expressions
  - [x] Write down datatypes for regular expressions (WITHOUT RANGES)
  - [x] Write down datatypes for NFA
  - [x] Implement NFA conversion
  - [x] Write down datatypes for DFA
  - [x] Fix the bug in NFA construction: I actually use multiple edges with the same label
  - [x] Implement NFA-to-DFA conversion
  - [x] Write a "library" to print Graphviz graphs to avoid concatenating strings
  - [x] Debug the DFA construction - figure out why it turns to be empty
  - [x] Implement printing DFA to Graphviz
  - [x] Implement automata minimization
  - [x] Refactor stuff in Regular.Expr to several modules
  - [x] Implement maximal munch interpretation
  - [x] Add token type labels to automata

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

## Bucket list

- Refactor regular expression compilation code: especially subset construction and DFA minimization are quite messy

## Useful CLI incantations

Using profiling from Cabal:

```
cabal run --enable-profiling lvar -- tests/cond.lvar +RTS -p
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
