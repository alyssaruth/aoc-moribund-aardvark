module Solutions.Day24 (aoc24) where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Common.BinaryUtils (pad0)
import Control.Lens (imap)
import Data.Bits (xor, (.&.), (.|.))
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (intercalate, isPrefixOf, sort, sortOn, (\\))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified Data.Ord
import Debug.Trace (traceShow)
import Numeric (showIntAtBase)
import Text.Parser.Char (alphaNum)
import Text.Trifecta (Parser, integer, letter, many, some, string, token, try)

type Wire = String

data WireEquation = WireEquation {leftWire :: Wire, rightWire :: Wire, operation :: String, destinationWire :: Wire}
  deriving (Eq, Show)

aoc24 :: IO ()
aoc24 = do
  printSolutions 24 'A' $ MkAoCSolution parseInput part1
  printSolutions 24 'B' $ MkAoCSolution parseInput part2

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
part1 (knownWires, equations) = getBinaryNumber "z" $ fromJust $ resolveSystem knownWires equations

resolveSystem :: M.Map Wire Int -> [WireEquation] -> Maybe (M.Map Wire Int)
resolveSystem knownWires wireEquations
  | null wireEquations = Just knownWires
  | null solvableEquations = Nothing -- Some sort of cycle has occurred
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

part2 :: (a, [WireEquation]) -> [Char]
part2 (_, equations) = intercalate "," $ sort $ fixAdder equations []

possibleSwaps :: [WireEquation] -> [[WireEquation]]
possibleSwaps equations = choose equations 2

choose :: [b] -> Int -> [[b]]
_ `choose` 0 = [[]]
[] `choose` _ = []
(x : xs) `choose` k = (x :) `fmap` (xs `choose` (k - 1)) ++ xs `choose` k

fixAdder :: [WireEquation] -> [Wire] -> [Wire]
fixAdder equations wiresSwapped
  | isNothing result = wiresSwapped
  | otherwise = traceShow ("Failed at depth " ++ show (fromJust result)) $ fixAdder newEquations (wiresSwapped ++ newWires)
  where
    maxDepth = calculateMaxDepth equations
    result = verifyAdder 0 maxDepth equations
    (newEquations, newWires) = fixDepth equations (fromMaybe 0 result) wiresSwapped

fixDepth :: [WireEquation] -> Int -> [Wire] -> ([WireEquation], [Wire])
fixDepth equations depth wiresPreviouslySwapped
  | null correctSwaps = error ("No suitable swap found to fix depth " ++ show depth)
  | otherwise = traceShow ("Swapped wires " ++ show swappedWires) (fixedMachine, swappedWires)
  where
    swapsToTest = prioritisedSwaps depth $ possibleSwaps $ swappableWires equations depth wiresPreviouslySwapped
    correctSwaps = traceShow ("Testing " ++ show (length swapsToTest) ++ " swaps") $ filter (testSwap equations (depth + 1)) swapsToTest
    correctSwap = head correctSwaps
    swappedWires = map destinationWire correctSwap
    fixedMachine = performSwap equations correctSwap

-- Prioritise swaps that involve zN or connected wires - these are more likely to be relevant to us
prioritisedSwaps :: Int -> [[WireEquation]] -> [[WireEquation]]
prioritisedSwaps depth swaps = sortOn (Data.Ord.Down . (\[a, b] -> includesWires interestingWires a || includesWires interestingWires b)) swaps
  where
    zWire = makeWire "z" depth
    znEquation = head $ filter ((== zWire) . destinationWire) $ map head swaps
    interestingWires = [zWire, leftWire znEquation, rightWire znEquation]

-- If we've failed at depth N, then we know that x0...xn-1, y0...yn-1 and z0...zn-1 are wired up right already
-- Likewise, anything immediately connected to x0...xn-2 and y0...yn-2, because even remainders of these are definitely working
-- We also know we can ignore any wires we've already swapped
swappableWires :: [WireEquation] -> Int -> [Wire] -> [WireEquation]
swappableWires equations failedDepth wiresPreviouslySwapped = filter (not . includesWires allWires) equations
  where
    xWires = map (makeWire "x") [0 .. (failedDepth - 1)]
    yWires = map (makeWire "y") [0 .. (failedDepth - 1)]
    zWires = map (makeWire "z") [0 .. (failedDepth - 1)]
    connectedToSafeWires = map destinationWire $ filter (includesWires (init xWires ++ init yWires)) equations
    allWires = xWires ++ yWires ++ zWires ++ wiresPreviouslySwapped ++ connectedToSafeWires

includesWires :: [Wire] -> WireEquation -> Bool
includesWires wires (WireEquation leftWire rightWire _ destinationWire) = leftWire `elem` wires || rightWire `elem` wires || destinationWire `elem` wires

testSwap :: [WireEquation] -> Int -> [WireEquation] -> Bool
testSwap equations depth swap = result
  where
    newMachine = performSwap equations swap
    result = isNothing (verifyAdderInReverse depth newMachine)

performSwap :: [WireEquation] -> [WireEquation] -> [WireEquation]
performSwap equations [a, b] = [newEquationA, newEquationB] ++ filtered
  where
    newEquationA = WireEquation (leftWire a) (rightWire a) (operation a) (destinationWire b)
    newEquationB = WireEquation (leftWire b) (rightWire b) (operation b) (destinationWire a)
    filtered = [equation | equation <- equations, equation /= a, equation /= b]

-- When verifying swaps, it's faster to go in reverse since the higher-order additions are more likely to fail
-- This is particularly true because we already exclude various wires from consideration that would break the lower-order additions
verifyAdderInReverse :: Int -> [WireEquation] -> Maybe Int
verifyAdderInReverse depth equations
  | not result = Just depth
  | depth == 0 = Nothing
  | otherwise = verifyAdderInReverse (depth - 1) equations
  where
    result = testBinaryAdder depth equations

-- Verify depths incrementally until one fails
verifyAdder :: Int -> Int -> [WireEquation] -> Maybe Int
verifyAdder depth maxDepth equations
  | not $ testBinaryAdder depth equations = Just depth
  | depth == maxDepth = Nothing
  | otherwise = verifyAdder (depth + 1) maxDepth equations

-- Test the binary adder at a specific "depth", by throwing a set of additions at it and verifying the results
testBinaryAdder :: Int -> [WireEquation] -> Bool
testBinaryAdder depth equations = all (testPasses equations) (prepareTestSums depth)

testPasses :: [WireEquation] -> (Int, Int) -> Bool
testPasses equations (x, y) = performAddition equations x y == Just (x + y)

prepareTestSums :: Int -> [(Int, Int)]
prepareTestSums depth =
  [ (minValue, 0),
    (minValue - 1, 1),
    (minValue, maxValue - minValue),
    (minValue `div` 2, maxValue `div` 2)
  ]
  where
    minValue = 2 ^ depth
    maxValue = 2 ^ (depth + 1) - 1

-- Set up inputs for X and Y to perform a specific addition
performAddition :: [WireEquation] -> Int -> Int -> Maybe Int
performAddition wireEquations x y = getBinaryNumber "z" <$> resolveSystem (M.union xWires yWires) wireEquations
  where
    maxDepth = calculateMaxDepth wireEquations
    xWires = prepareWires "x" maxDepth $ base2 x
    yWires = prepareWires "y" maxDepth $ base2 y

base2 :: Int -> String
base2 x = showIntAtBase 2 intToDigit x ""

calculateMaxDepth :: [WireEquation] -> Int
calculateMaxDepth machine = maximum [read $ filter isDigit wire | wire <- allInputWires machine, "x" `isPrefixOf` wire]

allInputWires :: [WireEquation] -> [Wire]
allInputWires equations = map leftWire equations ++ map rightWire equations

prepareWires :: String -> Int -> String -> M.Map Wire Int
prepareWires prefix maxDepth binaryValue = M.fromList newValues
  where
    keys = map (makeWire prefix) [0 .. maxDepth]
    newValues = map (\key -> (key, getNewValue binaryValue key)) keys

makeWire :: String -> Int -> Wire
makeWire prefix n = prefix ++ pad0 2 (show n)

getNewValue :: String -> Wire -> Int
getNewValue binaryValue wire
  | numericWire >= length binaryValue = 0
  | otherwise = digitToInt $ reverse binaryValue !! numericWire
  where
    numericWire = read $ filter isDigit wire