module Solutions.Day1 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Data.List
import Text.Trifecta
  ( Parser,
    TokenParsing (token),
    count,
    integer,
    some,
  )

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 'A' $ MkAoCSolution parseInput part1
  printSolutions 1 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser [(Integer, Integer)]
parseInput = do
  some $ token parsePair

parsePair :: Parser (Integer, Integer)
parsePair = do
  [a, b] <- count 2 integer
  pure (a, b)

part1 :: [(Integer, Integer)] -> Integer
part1 x = sum $ zipWith diff (sortedList fst x) (sortedList snd x)

sortedList :: ((Integer, Integer) -> Integer) -> [(Integer, Integer)] -> [Integer]
sortedList fn list = sort $ map fn list

diff :: Integer -> Integer -> Integer
diff x y = abs $ x - y

part2 :: [(Integer, Integer)] -> Integer
part2 x = sum $ map (similarityScore (sortedList snd x)) (sortedList fst x)

similarityScore :: [Integer] -> Integer -> Integer
similarityScore xs x = x * genericLength [a | a <- xs, a == x]
