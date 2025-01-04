# Advent of Code 2024

Solutions to 2024's Advent of Code, written in Haskell. Forked from RaphaelColman's excellent repo [here](https://github.com/RaphaelColman/AdventOfCodeTemplate).


## Benchmarking

Some of the solutions aren't terrible fast... below are those that are slower than 1s when run via stack ghci:

| Day | Time (s) |
|-----|----------|
| 9   | 62.46 |
| 22  | 47.47 |
| 24  | 27.00 |
| 6   | 13.07 |
| 7   | 9.56  |
| 14  | 7.08  |
| 15  | 6.57  |
| 16  | 6.20  |
| 20  | 5.56  |
| 17  | 2.72  |
| 25  | 1.67  |
| 19  | 1.50  |
| 21  | 1.14  |
| 5   | 1.04  |


## Running the solutions

To run:
```
❯ stack ghci
...

# Individual solution
λ> aoc1
1A: 5
1B: 7

# All solutions with benchmarking
λ> main
1A: 5
1B: 7
CPU time:   0.17s

2A: 100
2B: 250
CPU time:   0.04s
...
```

Alternatively, you can build and run the application completely:
```
❯ stack build
❯ stack exec aoc-moribund-aardvark-exe
1A: 5
1B: 7
CPU time:   0.17s

2A: 100
2B: 250
CPU time:   0.04s
...
```
