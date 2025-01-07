module Solutions.Day7 (aoc7) where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Data.List (isSuffixOf)
import Text.Trifecta (CharParsing (char, string), Parser, TokenParsing (token), integer, integer', sepBy, some)

data Equation = Equation {solution :: Integer, coefficients :: [Integer]}
  deriving (Show, Eq, Ord)

data Op = Plus | Times | Concatenate
  deriving (Eq, Show, Bounded, Enum, Ord)

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
part1 = sum . map solution . filter (canSolveEquation [Plus, Times])

canSolveEquation :: [Op] -> Equation -> Bool
canSolveEquation ops (Equation solution coefficients) = solveInReverse ops [solution] coefficients

solveInReverse :: [Op] -> [Integer] -> [Integer] -> Bool
solveInReverse ops currentTargets coefficients
  | length coefficients == 1 = nextCoefficient `elem` currentTargets
  | null currentTargets = False
  | otherwise = solveInReverse ops nextTargets (init coefficients)
  where
    nextCoefficient = last coefficients
    nextTargets = concatMap (applyValidOperations ops nextCoefficient) currentTargets

applyValidOperations :: [Op] -> Integer -> Integer -> [Integer]
applyValidOperations ops coefficient target = map (applyInverseOperation target coefficient) validOps
  where
    validOps = filter (isApplicable target coefficient) ops

applyInverseOperation :: Integer -> Integer -> Op -> Integer
applyInverseOperation target coefficient op
  | op == Plus = target - coefficient
  | op == Concatenate = read $ take (length (show target) - length (show coefficient)) (show target)
  | op == Times = target `div` coefficient

isApplicable :: Integer -> Integer -> Op -> Bool
isApplicable target coefficient op
  | op == Plus = target > coefficient
  | op == Concatenate = show coefficient `isSuffixOf` show target && coefficient /= target
  | op == Times = target `mod` coefficient == 0

part2 :: [Equation] -> Integer
part2 = sum . map solution . filter (canSolveEquation [Plus, Times, Concatenate])
