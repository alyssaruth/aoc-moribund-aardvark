module Solutions.Day21
  ( aoc21,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Text.Trifecta (Parser, anyChar, many, token, some, string, alphaNum, sepBy, letter, CharParsing (char), newline)
import Common.Geometry
import Linear (R1(_x), R2(_y), V2 (V2))
import Control.Lens ((^.))
import Data.List (nub)
import Data.Char (isDigit)
import Debug.Trace (traceShow)
import Common.ListUtils (window2)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)

type Inputs = [Char]
data Keypad = Keypad {layout :: Grid, arm :: Position, costsMap :: M.Map (Char, Char) Int}

data DistancePreference = X_FIRST | Y_FIRST | NEITHER
  deriving (Eq, Show, Bounded, Enum, Ord)

aoc21 :: IO ()
aoc21 = do
  printSolutions 21 $ MkAoCSolution parseInput part1
  printSolutions 21 $ MkAoCSolution parseInput part2

parseInput :: Parser [String]
parseInput = some $ token $ some alphaNum

codeEntryCombos :: Keypad -> Inputs -> [Inputs]
codeEntryCombos keyPad keys = enterCode keyPad keys [""]

enterCode :: Keypad -> Inputs -> [Inputs] -> [Inputs]
enterCode keyPad keys inputCombos
 | keys == "" = inputCombos
 | otherwise = enterCode newKeyPad (tail keys) nextInputs
  where
    nextKey = head keys
    newKeyPad = updateKeyPad nextKey keyPad
    nextInputs = concatMap (moveArm keyPad nextKey) inputCombos

moveArm :: Keypad -> Char -> Inputs -> [Inputs]
moveArm keyPad nextKey inputs
 | badX == (arm keyPad ^. _x) && badY == (yDelta + arm keyPad ^. _y) = [xFirst] -- Prevent moving vertically to empty space
 | badY == (arm keyPad ^. _y) && badX == (xDelta + arm keyPad ^. _x) = [yFirst] -- Prevent moving horizontally to empty space
 | distancePref == X_FIRST = [xFirst]
 | distancePref == Y_FIRST = [yFirst]
 | otherwise = [xFirst, yFirst]
  where
    (V2 badX badY) = locate '.' (layout keyPad)
    previousPosition = locate (last inputs) (layout keyPad)
    desiredPosition = locate nextKey (layout keyPad)
    xDelta = (desiredPosition ^. _x) - (arm keyPad ^. _x)
    yDelta = (desiredPosition ^. _y) - (arm keyPad ^. _y)
    xKeys = xDeltaToKeyPresses xDelta
    yKeys = yDeltaToKeyPresses yDelta
    xFirst = inputs ++ xKeys ++ yKeys ++ ['A']
    yFirst = inputs ++ yKeys ++ xKeys ++ ['A']
    distancePref = computeDistancePreference inputs keyPad xKeys yKeys

computeDistancePreference :: Inputs -> Keypad -> Inputs -> Inputs -> DistancePreference
computeDistancePreference inputsSoFar keyPad xKeys yKeys
  | xKeys == "" || yKeys == "" = X_FIRST -- Equivalent to Y_FIRST, just pick one
  | inputsSoFar == "" = NEITHER
  | xCost < yCost = X_FIRST
  | yCost < xCost = Y_FIRST
  | otherwise = NEITHER
  where
    previousKey = last inputsSoFar
    xKey = head xKeys
    yKey = head yKeys
    xCost = fromMaybe 0 $ M.lookup (previousKey, xKey) (costsMap keyPad)
    yCost = fromMaybe 0 $ M.lookup (previousKey, yKey) (costsMap keyPad)


updateKeyPad :: Char -> Keypad -> Keypad
updateKeyPad nextKey (Keypad layout _ costsMap) = Keypad layout (locate nextKey layout) costsMap

xDeltaToKeyPresses :: Int -> Inputs
xDeltaToKeyPresses delta = replicate (abs delta) key
  where
    key = if delta < 0 then '<' else '>'

yDeltaToKeyPresses :: Int -> Inputs
yDeltaToKeyPresses delta = replicate (abs delta) key
  where
    key = if delta < 0 then '^' else 'v'

shortestButtonPressSequence :: Int -> String -> Int
shortestButtonPressSequence directionalKeypads code = minimum $ map length $ addKeyPads directionalKeypads directionalKeyPad numericCombos
  where
    numericKeyPad = makeKeypad "789\n456\n123\n.0A"
    directionalKeyPad = makeKeypad ".^A\n<v>"
    numericCombos = codeEntryCombos numericKeyPad code

addKeyPads :: Int -> Keypad -> [Inputs] -> [Inputs]
addKeyPads number keyPad inputs
  | number == 0 = inputs
  | otherwise = addKeyPads (number-1) keyPad $ addKeyPad keyPad inputs

makeKeypad :: String -> Keypad
makeKeypad lines = Keypad grid (locate 'A' grid) (buildCostsMap grid)
  where
    grid = enumerateMultilineStringToVectorMap lines

complexity :: Int -> String -> Int
complexity keyPads code = numericPart * shortestButtonPressSequence keyPads code
  where
    numericPart = read $ filter isDigit code
    shortestSequence = shortestButtonPressSequence keyPads code

addKeyPad :: Keypad -> [Inputs] -> [Inputs]
addKeyPad keyPad inputs = traceShow (show (length newSequences) ++ "->" ++ show (length filteredSequences)) $ take 1 filteredSequences
  where
    newSequences = concatMap (codeEntryCombos keyPad) inputs
    minCost = minimum $ map (computeInputCost (costsMap keyPad)) newSequences
    filteredSequences = filter (\x -> computeInputCost (costsMap keyPad) x == minCost) newSequences


buildCostsMap :: Grid -> M.Map (Char, Char) Int
buildCostsMap grid = M.fromList $ map (\pair -> (pair, computeCost grid pair)) pairs
  where
    keys = filter (/='.') $ M.elems grid
    pairs = concatMap (keyPairs keys) keys

computeCost :: Grid -> (Char, Char) -> Int
computeCost layout (start, end) = abs (x1 - x2) + abs (y1 - y2)
  where
    (V2 x1 y1) = locate start layout
    (V2 x2 y2) = locate end layout

lookupCost :: M.Map (Char, Char) Int -> (Char, Char) -> Int
lookupCost costMap keyCombo = fromMaybe 0 result
  where
    result = M.lookup keyCombo costMap

keyPairs :: [Char] -> Char ->  [(Char, Char)]
keyPairs allKeys key = map (\other -> (key, other)) allKeys

computeInputCost :: M.Map (Char, Char) Int -> Inputs -> Int
computeInputCost costMap inputs = sum $ map (lookupCost costMap) $ window2 inputs


-- countDirectionChanges :: Inputs -> Int
-- countDirectionChanges seq = length $ filter (uncurry (/=)) $ window2 seq

part1 :: [String] -> Int
part1 codes = sum $ map (complexity 2) codes

part2 :: [String] -> Int
part2 codes = shortestButtonPressSequence 4 "029A"
