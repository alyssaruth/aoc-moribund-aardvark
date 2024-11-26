module Solutions.Day1
  ( aoc1
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.List           (tails)
import           Data.Char           (isDigit)
import           Text.Trifecta       (Parser, TokenParsing (token), letter,
                                      some)
import Common.ListUtils (window3, window2)

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

type CalibrationLines = [String]

parseInput :: Parser CalibrationLines
parseInput = do
  some $ token $ some letter

part1 :: CalibrationLines -> Int
part1 x = sum (map calibrationValue x)

part2 :: CalibrationLines -> Int
part2 x = 10

calibrationValue :: String -> Int
calibrationValue x = read ([(firstNumber x)] ++ [(lastNumber x)])

firstNumber :: String -> Char
firstNumber x = (head ( filter isDigit x ) )

lastNumber :: String -> Char
lastNumber x = firstNumber ( reverse x )
