module Solutions.Day19
  ( aoc19,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Text.Trifecta (Parser, alphaNum, letter, manyTill, sepBy, some, string, token)

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

updateMapForDesign :: M.Map Design Int -> [Towel] -> Design -> M.Map Design Int
updateMapForDesign knownCombos towels design
  | M.member design knownCombos = knownCombos
  | design `elem` towels = M.insert design (1 + newSum) knownCombos
  | otherwise = M.insert design newSum knownCombos
  where
    potentialTowels = [towel | towel <- towels, towel `isPrefixOf` design]
    subDesigns = map (subDesign design) potentialTowels
    results = mapMaybe (`M.lookup` knownCombos) subDesigns
    newSum = sum results

subDesign :: Design -> Towel -> Design
subDesign design towel = drop (length towel) design

processDesignBackwards :: M.Map Design Int -> [Towel] -> Design -> Int -> M.Map Design Int
processDesignBackwards map towels design tailLength
  | tailLength == length design = updatedMap
  | otherwise = processDesignBackwards updatedMap towels design (tailLength + 1)
  where
    subDesign = reverse $ take tailLength (reverse design)
    updatedMap = updateMapForDesign map towels subDesign

processDesigns :: [Towel] -> [Design] -> M.Map Design Int -> M.Map Design Int
processDesigns towels [] map = map
processDesigns towels designs map = processDesigns towels (tail designs) updatedMap
  where
    updatedMap = processDesignBackwards map towels (head designs) 1

countDesignCombos :: ([Towel], [Design]) -> [Int]
countDesignCombos (towels, designs) = mapMaybe (`M.lookup` allCounts) designs
  where
    allCounts = processDesigns towels designs M.empty

part1 :: ([Towel], [Design]) -> Int
part1 = length . filter (> 0) . countDesignCombos

part2 :: ([Towel], [Design]) -> Int
part2 = sum . countDesignCombos
