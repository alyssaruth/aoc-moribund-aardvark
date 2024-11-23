# Advent of Code 2024

Solutions to 2024's Advent of Code, written in Haskell. Forked from RaphaelColman's excellent repo [here](https://github.com/RaphaelColman/AdventOfCodeTemplate)

# Table of Contents
- [Advent of Code Template](#advent-of-code-template)
- [Table of Contents](#table-of-contents)
    - [Overview](#overview)
    - [Getting started](#getting-started)



To run, you can use the GHCI repl. For example:
```
❯ stack ghci
Using main module: 1. Package `AdventOfCode2022' component AdventOfCode2022:exe:AdventOfCode2022-exe with main-is file: /Users/raphael.colman/Dev/AdventOfCode2022/app/Main.hs
AdventOfCode2022> initial-build-steps (lib + exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: AdventOfCode2022
...

λ> aoc1
Solution:
1602
Solution:
1633
```

This is good for trialling solutions, because `:r` will reload your changes. You can also use the `printTestSolutions` function to use inputs from /res/test_inputs instead

Alternatively, you can build and run the application completely
```
❯ stack build
❯ stack exec AdventOfCode2022-exe
Which day of Advent do you want to solve? [1-25]
1
Solution:
1602
Solution:
1633
```
