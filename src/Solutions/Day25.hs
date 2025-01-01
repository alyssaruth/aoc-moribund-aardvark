module Solutions.Day25
  ( aoc25
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, anyChar, many)
import Common.Geometry
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Linear (V2(..))
import Data.List (partition)

type Key = Grid
type Lock = Grid

aoc25 :: IO ()
aoc25 = do
  printSolutions 25 $ MkAoCSolution parseInput part1
  printSolutions 25 $ MkAoCSolution parseInput part2

parseInput :: Parser ([Key], [Lock])
parseInput = do
  allChars <- many anyChar
  let grids = map enumerateMultilineStringToVectorMap $ splitOn "\n\n" allChars
  pure $ partition isKey grids

part1 :: ([Key], [Lock]) -> Int
part1 (keys, locks) = length [key | key <- keys, lock <- locks, fits key lock]

isKey :: Grid -> Bool
isKey grid = all (=='#') [value | (V2 x y, value) <- M.toList grid, y == 0]

fits :: Key -> Lock -> Bool
fits key lock = all ((<8) . uncurry (+)) (zip (heights key) (heights lock))

heights :: Grid -> [Int]
heights grid = map (height grid) [0..4]

height :: Grid -> Int -> Int
height grid xValue = length [value | (V2 x y, value) <- M.toList grid, x == xValue, value == '#']

part2 :: ([Key], [Lock]) -> String
part2 = const "Merry Christmas!"
