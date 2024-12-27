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

codeEntryCombos :: Keypad -> Inputs -> [Inputs]
codeEntryCombos keyPad keys = enterCode keyPad keys [""]

enterCode :: Keypad -> Inputs -> [Inputs] -> [Inputs]
enterCode keyPad keys inputCombos
 | keys == "" = inputCombos
 | otherwise = enterCode newKeyPad (tail keys) inputsToCurrentKey
  where
    nextKey = head keys
    (newKeyPad, nextInputs) = moveArm keyPad nextKey
    inputsToCurrentKey = concatMap (appendInputs nextInputs) inputCombos
    --minCost = minimum $ map (computeInputCost $ costsMap keyPad) inputsToCurrentKey
    --doubleFilteredSequences = filter (\x -> computeInputCost (costsMap keyPad) x == minCost) inputsToCurrentKey

appendInputs :: [Inputs] -> Inputs -> [Inputs]
appendInputs newInputs currentInputs = map (currentInputs ++) newInputs

moveArm :: Keypad -> Char -> (Keypad, [Inputs])
moveArm keyPad nextKey
 | desiredPosition == arm keyPad = (keyPad, ["A"])
 | badX == (arm keyPad ^. _x) && badY == (yDelta + arm keyPad ^. _y) = (newKeyPad, [xKeys ++ yKeys ++ ['A']])
 | badY == (arm keyPad ^. _y) && badX == (xDelta + arm keyPad ^. _x) = (newKeyPad, [yKeys ++ xKeys ++ ['A']])
 | otherwise = (newKeyPad, nub [xKeys ++ yKeys ++ ['A'], yKeys ++ xKeys ++ ['A']])
  where
    (V2 badX badY) = locate '.' (layout keyPad)
    desiredPosition = locate nextKey (layout keyPad)
    xDelta = (desiredPosition ^. _x) - (arm keyPad ^. _x)
    yDelta = (desiredPosition ^. _y) - (arm keyPad ^. _y)
    xKeys = xDeltaToKeyPresses xDelta
    yKeys = yDeltaToKeyPresses yDelta
    newKeyPad = Keypad (layout keyPad) desiredPosition (costsMap keyPad)

xDeltaToKeyPresses :: Int -> Inputs
xDeltaToKeyPresses delta = replicate (abs delta) key
  where
    key = if delta < 0 then '<' else '>'

yDeltaToKeyPresses :: Int -> Inputs
yDeltaToKeyPresses delta = replicate (abs delta) key
  where
    key = if delta < 0 then '^' else 'v'

aoc21 :: IO ()
aoc21 = do
  printSolutions 21 $ MkAoCSolution parseInput part1
  printSolutions 21 $ MkAoCSolution parseInput part2

parseInput :: Parser [String]
parseInput = some $ token $ some alphaNum

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
--addKeyPad keyPad inputs = traceShow (show (length newSequences) ++ "->" ++ show (length doubleFilteredSequences)) $ take 1 doubleFilteredSequences
addKeyPad keyPad inputs = newSequences
  where
    newSequences = concatMap (codeEntryCombos keyPad) inputs
    -- minCost = minimum $ map (computeInputCost costsMap) newSequences
    -- doubleFilteredSequences = filter (\x -> computeInputCost costsMap x == minCost) newSequences


buildCostsMap :: Grid -> M.Map (Char, Char) Int
buildCostsMap grid = M.fromList $ map (\pair -> (pair, computeCost grid pair)) pairs
  where
    keys = filter (/='.') $ M.elems grid
    pairs = concatMap (keyPairs keys) keys

computeCost :: Grid -> (Char, Char) -> Int
computeCost layout (start, end) = length $ head options
  where
    keyPad = Keypad layout (locate start layout) M.empty
    (_, options) = moveArm keyPad end

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
part2 codes = undefined
