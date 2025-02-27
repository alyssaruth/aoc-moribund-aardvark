# Advent of Code 2024

Solutions to 2024's Advent of Code, written in Haskell. Forked from RaphaelColman's excellent repo [here](https://github.com/RaphaelColman/AdventOfCodeTemplate).


## Benchmarking

Some of the solutions aren't terribly fast... below are those that are slower than 1s when run via GHCI:

| Day | Time (s) |
|-----|----------|
| 22  | 47.47 |
| 6   | 13.07 |
| 24  | 10.08 |
| 14  | 7.08  |
| 15  | 6.57  |
| 20  | 5.56  |
| 9   | 3.56  |
| 17  | 2.72  |
| 16  | 1.83  |
| 19  | 1.50  |
| 21  | 1.14  |


## Running the solutions

To run:
```
❯ stack ghci

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
