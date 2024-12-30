module Solutions.Day24
  ( aoc24,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Control.Lens (imap)
import Data.Bits (xor, (.&.), (.|.))
import Data.List (isPrefixOf, sortBy, sortOn, (\\))
import qualified Data.Map as M
import qualified Data.Ord
import Text.Parser.Char (alphaNum)
import Text.Trifecta (Parser, integer, letter, many, some, string, token, try)

type Wire = String

data WireEquation = WireEquation {leftWire :: Wire, rightWire :: Wire, operation :: String, destinationWire :: Wire}
  deriving (Eq)

aoc24 :: IO ()
aoc24 = do
  printSolutions 24 $ MkAoCSolution parseInput part1
  printSolutions 24 $ MkAoCSolution parseInput part2

parseInput :: Parser (M.Map Wire Int, [WireEquation])
parseInput = do
  knownWires <- many $ try parseInputWire
  equations <- some $ token parseEquation
  pure (M.fromList knownWires, equations)

parseInputWire :: Parser (Wire, Int)
parseInputWire = do
  wire <- some alphaNum <* string ": "
  value <- integer
  pure (wire, fromInteger value)

parseEquation :: Parser WireEquation
parseEquation = do
  leftWire <- some alphaNum <* string " "
  fn <- some letter <* string " "
  rightWire <- some alphaNum <* string " "
  destination <- string "-> " *> some alphaNum
  pure $ WireEquation leftWire rightWire fn destination

parseOperator :: String -> (Int -> Int -> Int)
parseOperator "XOR" = xor
parseOperator "OR" = (.|.)
parseOperator "AND" = (.&.)

part1 :: (M.Map Wire Int, [WireEquation]) -> Int
part1 (knownWires, equations) = getOutput $ resolveSystem knownWires equations

resolveSystem :: M.Map Wire Int -> [WireEquation] -> M.Map Wire Int
resolveSystem knownWires wireEquations
  | null wireEquations = knownWires
  | otherwise = resolveSystem newMap (wireEquations \\ solvableEquations)
  where
    solvableEquations = filter (isSolvable knownWires) wireEquations
    newPairs = map (solveEquation knownWires) solvableEquations
    newMap = M.union knownWires (M.fromList newPairs)

isSolvable :: M.Map Wire Int -> WireEquation -> Bool
isSolvable knownWires (WireEquation leftWire rightWire _ _) = leftWire `M.member` knownWires && rightWire `M.member` knownWires

solveEquation :: M.Map Wire Int -> WireEquation -> (Wire, Int)
solveEquation knownWires (WireEquation leftWire rightWire operation destinationWire) = (destinationWire, result)
  where
    operator = parseOperator operation
    result = (knownWires M.! leftWire) `operator` (knownWires M.! rightWire)

getOutput :: M.Map Wire Int -> Int
getOutput outputs = sum $ imap toDecimalPart zOutputs
  where
    zOutputs = map snd $ sortOn fst (filter (\(wire, value) -> "z" `isPrefixOf` wire) $ M.toList outputs)

toDecimalPart :: Int -> Int -> Int
toDecimalPart ix value = (2 ^ ix) * value

part2 :: (M.Map Wire Int, [WireEquation]) -> String
part2 = undefined
