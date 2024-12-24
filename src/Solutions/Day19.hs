module Solutions.Day19
  ( aoc19,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Text.Trifecta (Parser, alphaNum, letter, manyTill, sepBy, some, string, token)
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)

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
  | design `elem` towels = M.insert design (1+newSum) knownCombos
  | null potentialTowels = M.insert design 0 knownCombos
  | otherwise = M.insert design newSum knownCombos
  where
    potentialTowels = [towel | towel <- towels, towel `isPrefixOf` design]
    subDesigns = map (subDesign design) potentialTowels
    results = mapMaybe (`M.lookup` knownCombos) subDesigns
    newSum = sum results

subDesign :: Design -> Towel -> Design
subDesign design towel = drop (length towel) design

processDesignBackwards :: M.Map Design Int -> [Towel] -> Design -> Design -> M.Map Design Int
processDesignBackwards map towels design subDesign
  | subDesign == design = updatedMap
  | otherwise = processDesignBackwards updatedMap towels design nextSubDesign
  where
    nextSubDesign = drop (length design - length subDesign - 1) design
    updatedMap = updateMapForDesign map towels subDesign

processDesigns :: [Towel] -> [Design] -> M.Map Design Int -> M.Map Design Int
processDesigns towels [] map = map
processDesigns towels designs map = processDesigns towels (tail designs) updatedMap
  where
    updatedMap = processDesignBackwards map towels (head designs) [last $ head designs]

part1 :: ([Towel], [Design]) -> Int
part1 (towels, designs) = length $ filter (>0) $ mapMaybe (`M.lookup` allCounts) designs
  where
    allCounts = processDesigns towels designs M.empty

part2 :: ([Towel], [Design]) -> Int
part2 (towels, designs) = sum $ mapMaybe (`M.lookup` allCounts) designs
  where
    allCounts = processDesigns towels designs M.empty
