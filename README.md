# Adventures in Compilation

Instead of trying to "implement a cool programming language", a project that I have started maybe a dozen times and never got far with, the idea here is to play with different algorithms of compilation on extremely simplistic languages.

## Adventure 1: simple SSA construction

- [x] Implement compilation of statements into blocks
- [x] Implement compilation of functions into blocks
- [ ] Implement traversal on the blocks to find the unreachable ones
- [ ] Using the previous item, implement "unreachable code" analysis; this might need backwards mapping from blocks to statements
- [ ] Only then start adding interesting control flow (conditionals and while-loops)
