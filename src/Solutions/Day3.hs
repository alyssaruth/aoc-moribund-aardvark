module Solutions.Day3
  ( aoc3,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Text.Parser.Combinators
import Text.Regex.TDFA ((=~))
import Text.Trifecta (CharParsing (anyChar), Parser)

aoc3 :: IO ()
aoc3 = do
  printSolutions 3 $ MkAoCSolution parseInput part1
  printSolutions 3 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = many anyChar

multiplicationRegex = "mul\\(([0-9]+),([0-9]+)\\)"

part1 :: String -> Integer
part1 x = sum $ map computeMultResult (x =~ multiplicationRegex :: [[String]])

computeMultResult :: [String] -> Integer
computeMultResult x = product $ map read (tail x)

part2 :: String -> Integer
part2 x = undefined
