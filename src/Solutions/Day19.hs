module Solutions.Day19
  ( aoc19,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Text.Trifecta (Parser, alphaNum, anyChar, commaSep, letter, many, manyTill, sepBy, some, string, token, try)
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace

type Stripe = Char

type Towel = [Char]

type Design = [Char]

aoc19 :: IO ()
aoc19 = do
  printSolutions 19 $ MkAoCSolution parseInput part1
  printSolutions 19 $ MkAoCSolution parseInput part2

parseInput :: Parser ([Towel], [Design])
parseInput = do
  towels <- sepBy (some letter) (string ", ") <* string "\n\n"
  designs <- some $ token $ manyTill alphaNum (string "\n")
  pure (towels, designs)

countPossibilities :: M.Map Design Int -> [Towel] -> Design -> (M.Map Design Int, Int)
countPossibilities knownCombos towels design
  | M.member design knownCombos = (M.empty, fromJust $ M.lookup design knownCombos)
  | null design = (knownCombos, 1)
  | null potentialTowels = (M.insert design 0 knownCombos, 0)
  | otherwise = (M.insert design newSum newMap, newSum)
  where
    potentialTowels = [towel | towel <- towels, towel `isPrefixOf` design]
    subDesigns = map (subDesign design) potentialTowels
    results = map (countPossibilities knownCombos towels) subDesigns
    newMap = M.unions (knownCombos : map fst results)
    newSum = sum $ map snd results

subDesign :: Design -> Towel -> Design
subDesign design towel = drop (length towel) design

warmMap :: M.Map Design Int -> [Towel] -> Design -> Design -> M.Map Design Int
warmMap map towels design subDesign
  | subDesign == design = updatedMap
  | M.member subDesign map = warmMap map towels design nextSubDesign
  | otherwise = warmMap updatedMap towels design nextSubDesign
  where
    nextSubDesign = drop (length design - length subDesign - 1) design
    (updatedMap, _) = countPossibilities map towels subDesign

makeMap :: [Towel] -> [Design] -> M.Map Design Int -> M.Map Design Int
makeMap towels [] map = map
makeMap towels designs map = makeMap towels (tail designs) updatedMap
  where
    updatedMap = warmMap map towels (head designs) [last $ head designs]

part1 :: ([Towel], [Design]) -> Int
part1 (towels, designs) = length $ filter (>0) $ mapMaybe (`M.lookup` initialMap) designs
  where
    initialMap = makeMap towels designs M.empty

part2 :: ([Towel], [Design]) -> Int
part2 (towels, designs) = sum $ mapMaybe (`M.lookup` initialMap) designs
  where
    initialMap = makeMap towels designs M.empty
