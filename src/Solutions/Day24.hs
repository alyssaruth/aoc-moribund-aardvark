module Solutions.Day24
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
import Numeric (showIntAtBase)
import Data.Char (intToDigit, isDigit, digitToInt)
import Debug.Trace (traceShow)

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
part1 (knownWires, equations) = getBinaryNumber "z" $ resolveSystem knownWires equations

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

getBinaryNumber :: String -> M.Map Wire Int -> Int
getBinaryNumber prefix wireValues = sum $ imap toDecimalPart $ rawValues prefix wireValues

rawValues :: String -> M.Map Wire Int -> [Int]
rawValues prefix wireValues = map snd $ sortOn fst $ wirePairs prefix wireValues

wirePairs :: String -> M.Map Wire Int -> [(Wire, Int)]
wirePairs prefix wireValues = filter (\(wire, value) -> prefix `isPrefixOf` wire) $ M.toList wireValues

toDecimalPart :: Int -> Int -> Int
toDecimalPart ix value = (2 ^ ix) * value

part2 :: (M.Map Wire Int, [WireEquation]) -> String
part2 (knownWires, equations) = performAddition knownWires equations 15 23

-- doOneDigitAdditions :: M.Map Wire Int -> [WireEquation] -> Int
-- doOneDigitAdditions wires equations = undefined
--   where
--     pairs = sub


base2 :: Int -> String
base2 x = showIntAtBase 2 intToDigit x ""


performAddition :: M.Map Wire Int -> [WireEquation] -> Int -> Int -> String
performAddition originalWires wireEquations x y = show x ++ " + " ++ show y ++ " = " ++ show (getBinaryNumber "z" solution)
  where
    xWires = prepareWires originalWires "x" $ base2 x
    yWires = prepareWires originalWires "y" $ base2 y
    solution = resolveSystem (M.union xWires yWires) wireEquations

prepareWires :: M.Map Wire Int -> String -> String -> M.Map Wire Int
prepareWires originalWires prefix binaryValue = M.fromList newValues
  where
    keys = map fst (wirePairs prefix originalWires)
    newValues = map (\key -> (key, getNewValue binaryValue key)) keys

getNewValue :: String -> Wire -> Int
getNewValue binaryValue wire
  | numericWire >= length binaryValue = 0
  | otherwise = digitToInt $ reverse binaryValue !! numericWire
  where
    numericWire = read $ filter isDigit wire