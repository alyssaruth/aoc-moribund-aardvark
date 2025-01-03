module Solutions.Day13 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Text.Trifecta (CharParsing (string), Parser, TokenParsing (token), count, integer, some)

data ClawEquation = ClawEquation {a :: Integer, b :: Integer, total :: Integer}

type SimultaneousEquations = (ClawEquation, ClawEquation)

type Solution = (Integer, Integer)

aoc13 :: IO ()
aoc13 = do
  printSolutions 13 'A' $ MkAoCSolution parseInput part1
  printSolutions 13 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser [SimultaneousEquations]
parseInput = do
  some $ token parseEquation

parseEquation :: Parser SimultaneousEquations
parseEquation = do
  a1 <- string "Button A: X+" *> integer
  a2 <- string ", Y+" *> integer
  b1 <- string "Button B: X+" *> integer
  b2 <- string ", Y+" *> integer
  total1 <- string "Prize: X=" *> integer
  total2 <- string ", Y=" *> integer
  pure (ClawEquation a1 b1 total1, ClawEquation a2 b2 total2)

part1 :: [SimultaneousEquations] -> Integer
part1 = sum . map (tokenCost . solve)

part2 :: [SimultaneousEquations] -> Integer
part2 = sum . map (tokenCost . solve . updatePrize)

times :: Integer -> ClawEquation -> ClawEquation
times x (ClawEquation a b total) = ClawEquation (a * x) (b * x) (total * x)

plugInB :: Integer -> ClawEquation -> Integer
plugInB bValue (ClawEquation a b total) = (total - b * bValue) `div` a

solve :: SimultaneousEquations -> Maybe Solution
solve (eq1, eq2) = if bTotal `rem` bMultiplier /= 0 then Nothing else Just (plugInB bValue eq1, bValue)
  where
    aLcm = lcm (a eq1) (a eq2)
    newEq1 = (aLcm `div` a eq1) `times` eq1
    newEq2 = (aLcm `div` a eq2) `times` eq2
    bTotal = total newEq2 - total newEq1
    bMultiplier = b newEq2 - b newEq1
    bValue = bTotal `div` bMultiplier

updatePrize :: SimultaneousEquations -> SimultaneousEquations
updatePrize (eq1, eq2) = (updateTotal eq1, updateTotal eq2)

updateTotal :: ClawEquation -> ClawEquation
updateTotal (ClawEquation a b total) = ClawEquation a b (10000000000000 + total)

tokenCost :: Maybe Solution -> Integer
tokenCost Nothing = 0
tokenCost (Just (a, b)) = 3 * a + b
