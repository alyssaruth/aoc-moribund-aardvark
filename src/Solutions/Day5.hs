module Solutions.Day5
  ( aoc5,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Data.List (partition, sortBy)
import qualified Data.Set as S
import Text.Trifecta
  ( Parser,
    Parsing (eof, try),
    char,
    commaSep,
    integer,
    many,
    manyTill,
    sepBy,
  )

aoc5 :: IO ()
aoc5 = do
  printSolutions 5 'A' $ MkAoCSolution parseInput part1
  printSolutions 5 'B' $ MkAoCSolution parseInput part2

type Page = [Integer]

type PageRule = (Integer, Integer)

parseInput :: Parser (S.Set PageRule, S.Set Page)
parseInput = do
  pageRules <- many $ try parsePageRule
  pages <- manyTill (commaSep integer) eof
  pure (S.fromList pageRules, S.fromList pages)

parsePageRule :: Parser (Integer, Integer)
parsePageRule = do
  [a, b] <- sepBy integer (char '|')
  pure (a, b)

part1 :: (S.Set PageRule, S.Set Page) -> Integer
part1 = middlePageSum True

part2 :: (S.Set PageRule, S.Set Page) -> Integer
part2 = middlePageSum False

ruleSort :: S.Set PageRule -> Integer -> Integer -> Ordering
ruleSort rules x y
  | (x, y) `S.member` rules = LT
  | (y, x) `S.member` rules = GT
  | otherwise = EQ

middleElement :: Page -> Integer
middleElement xs = xs !! (length xs `div` 2)

middlePageSum :: Bool -> (S.Set PageRule, S.Set Page) -> Integer
middlePageSum sorted (rules, pages)
  | sorted = sum $ map middleElement sortedPages
  | otherwise = sum $ map middleElement unsortedPages
  where
    correctPages = S.toList $ S.map (sortBy (ruleSort rules)) pages
    (sortedPages, unsortedPages) = partition (`S.member` pages) correctPages