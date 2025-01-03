module Solutions.Day5
  ( aoc5,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Data.List
import Data.Maybe
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

parseInput :: Parser ([PageRule], [Page])
parseInput = do
  pageRules <- many $ try parsePageRule
  pages <- manyTill (commaSep integer) eof
  pure (pageRules, pages)

parsePageRule :: Parser (Integer, Integer)
parsePageRule = do
  [a, b] <- sepBy integer (char '|')
  pure (a, b)

part1 :: ([PageRule], [Page]) -> Integer
part1 (rules, pages) = sum $ map middleElement $ filter (`elem` pages) sortedPages
  where
    sortedPages = map (sortBy (ruleSort rules)) pages

ruleSort :: [PageRule] -> Integer -> Integer -> Ordering
ruleSort rules x y = compare (tupleIndex x relevantRule) (tupleIndex y relevantRule)
  where
    relevantRule = fromMaybe (-1, -1) $ find (containsBoth x y) rules

containsBoth :: Integer -> Integer -> PageRule -> Bool
containsBoth x y rule = rule == (x, y) || rule == (y, x)

tupleIndex :: Integer -> (Integer, Integer) -> Maybe Int
tupleIndex x (a, b) = elemIndex x [a, b]

middleElement :: Page -> Integer
middleElement [x] = x
middleElement xs = middleElement $ init $ tail xs

part2 :: ([PageRule], [Page]) -> Integer
part2 (rules, pages) = sum $ map middleElement $ filter (not . (`elem` pages)) sortedPages
  where
    sortedPages = map (sortBy (ruleSort rules)) pages
