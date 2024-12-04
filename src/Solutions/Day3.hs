module Solutions.Day3
  ( aoc3,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.ListUtils (window2, windowN)
import Data.List (elemIndices, sort)
import Text.Parser.Combinators (many)
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
part1 = sumMultiplications

sumMultiplications :: String -> Integer
sumMultiplications x = sum $ map computeMultResult (x =~ multiplicationRegex)

computeMultResult :: [String] -> Integer
computeMultResult x = product $ map read (tail x)

part2 :: String -> Integer
part2 x = sum $ map (sumMultiplications . slice x) validSegments
  where
    dos = 0 : findSubstrIndices x "do()"
    dosAndDonts = sort $ dos ++ findSubstrIndices x "don't()" ++ [length x]
    validSegments = filter (flip elem dos . fst) (window2 dosAndDonts)

slice :: [a] -> (Int, Int) -> [a]
slice xs (i, k) = [xs !! n | n <- [0 .. length xs - 1], n >= i, n < k]

findSubstrIndices :: String -> String -> [Int]
findSubstrIndices s target = elemIndices target (windowN (length target) s)
