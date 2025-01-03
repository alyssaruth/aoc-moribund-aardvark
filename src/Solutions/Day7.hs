module Solutions.Day7 (aoc7) where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Text.Trifecta (CharParsing (char, string), Parser, TokenParsing (token), integer, integer', sepBy, some)

data Equation = Equation {solution :: Integer, coefficients :: [Integer]}
  deriving (Show, Eq, Ord)

type Operation = Integer -> Integer -> Integer

aoc7 :: IO ()
aoc7 = do
  printSolutions 7 'A' $ MkAoCSolution parseInput part1
  printSolutions 7 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser [Equation]
parseInput = do
  some $ token parseEquation

parseEquation :: Parser Equation
parseEquation = do
  result <- integer <* string ": "
  coefficients <- sepBy integer' (char ' ')
  pure $ Equation result coefficients

part1 :: [Equation] -> Integer
part1 = sum . map solution . filter (canSolveEquation [(+), (*)])

canSolveEquation :: [Operation] -> Equation -> Bool
canSolveEquation ops e = solution e `elem` results
  where
    results = generateResults ops $ coefficients e

generateResults :: [Operation] -> [Integer] -> [Integer]
generateResults _ [x] = [x]
generateResults ops x = concatMap (generateResults ops . applyOperation x) ops

applyOperation :: [Integer] -> Operation -> [Integer]
applyOperation (a : b : xs) op = op a b : xs

concatenate :: Integer -> Integer -> Integer
concatenate x y = read $ show x ++ show y

part2 :: [Equation] -> Integer
part2 = sum . map solution . filter (canSolveEquation [(+), (*), concatenate])
