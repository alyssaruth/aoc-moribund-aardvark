module Solutions.Day2 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.ListUtils (window2)
import Data.Ix (inRange)
import Data.List
import qualified Data.Set as Set
import Text.Parser.Char
import Text.Parser.Token (integer')
import Text.Trifecta
  ( Parser,
    TokenParsing (token),
    sepBy,
  )

aoc2 :: IO ()
aoc2 = do
  printSolutions 2 $ MkAoCSolution parseInput part1
  printSolutions 2 $ MkAoCSolution parseInput part2

parseInput :: Parser [[Integer]]
parseInput = do
  xs <- sepBy (sepBy integer' (char ' ')) newline
  pure $ filter (not . null) xs

part1 :: [[Integer]] -> Integer
part1 = genericLength . filter safeReport

safeReport :: [Integer] -> Bool
safeReport x = length (Set.fromList signs) == 1 && all isGradual differences
  where
    differences = map (uncurry (-)) (window2 x)
    signs = map signum differences

isGradual :: Integer -> Bool
isGradual = inRange (1, 3) . abs

part2 :: [[Integer]] -> Integer
part2 = genericLength . filter mostlySafeReport

mostlySafeReport :: [Integer] -> Bool
mostlySafeReport x = any safeReport subReports
  where
    subReports = x : map (deleteNth x) [0 .. genericLength x - 1]

deleteNth :: [Integer] -> Int -> [Integer]
deleteNth xs n = a ++ drop 1 b
  where
    (a, b) = splitAt n xs