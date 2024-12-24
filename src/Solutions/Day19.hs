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

isPossible :: [Towel] -> Design -> Bool
isPossible towels design
  | null design = True
  | null potentialTowels = False
  | otherwise = any (isPossible towels) subDesigns
  where
    potentialTowels = [towel | towel <- towels, towel `isPrefixOf` design]
    subDesigns = map (subDesign design) potentialTowels

subDesign :: Design -> Towel -> Design
subDesign design towel = drop (length towel) design

part1 :: ([Towel], [Design]) -> Int
part1 (towels, designs) = length $ filter (isPossible towels) designs

part2 :: ([Towel], [Design]) -> String
part2 = undefined
