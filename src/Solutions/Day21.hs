{-# LANGUAGE TupleSections #-}
module Solutions.Day21
  ( aoc21,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Text.Trifecta (Parser, token, some, alphaNum)
import Common.Geometry
import Linear (R1(_x), R2(_y), V2 (V2))
import Control.Lens ((^.))
import Data.List (nub)
import Data.Char (isDigit)
import Debug.Trace (traceShow)
import qualified Data.Map as M

type Inputs = [Char]
data Keypad = Keypad {arm :: Char, optionsMap :: M.Map (Char, Char) [Inputs]}

aoc21 :: IO ()
aoc21 = do
  printSolutions 21 $ MkAoCSolution parseInput part1
  printSolutions 21 $ MkAoCSolution parseInput part2

parseInput :: Parser [String]
parseInput = some $ token $ some alphaNum

getKeySequence :: Keypad -> Inputs -> Inputs
getKeySequence keyPad keys = enterCode keyPad keys ""

enterCode :: Keypad -> Inputs -> Inputs -> Inputs
enterCode keyPad keys inputsSoFar
 | keys == "" = inputsSoFar
 | otherwise = enterCode newKeyPad (tail keys) nextInputs
  where
    nextKey = head keys
    newKeyPad = Keypad nextKey (optionsMap keyPad)
    nextInputs = moveArm keyPad nextKey inputsSoFar

moveArm :: Keypad -> Char -> Inputs -> Inputs
moveArm keyPad nextKey inputs = inputs ++ nextInputs keyPad nextKey inputs

nextInputs :: Keypad -> Char -> Inputs -> Inputs
nextInputs keyPad nextKey inputs
 | length options == 1 = head options
 | otherwise = distancePref
  where
    options = optionsMap keyPad M.! (arm keyPad, nextKey)
    distancePref = computeDistancePreference options

computeDistancePreference :: [Inputs] -> Inputs
computeDistancePreference [xFirst, yFirst]
  | xKey == '<' = xFirst
  | otherwise = yFirst
  where
    xKey = head xFirst
    yKey = head yFirst

xDeltaToKeyPresses :: Int -> Inputs
xDeltaToKeyPresses delta = replicate (abs delta) key
  where
    key = if delta < 0 then '<' else '>'

yDeltaToKeyPresses :: Int -> Inputs
yDeltaToKeyPresses delta = replicate (abs delta) key
  where
    key = if delta < 0 then '^' else 'v'

shortestButtonPressSequence :: Int -> String -> Int
shortestButtonPressSequence directionalKeypads code = length $ addKeyPads directionalKeypads directionalKeyPad numericCombo
  where
    numericKeyPad = makeKeypad "789\n456\n123\n.0A"
    directionalKeyPad = makeKeypad ".^A\n<v>"
    numericCombo = getKeySequence numericKeyPad code

addKeyPads :: Int -> Keypad -> Inputs -> Inputs
addKeyPads number keyPad inputs
  | number == 0 = inputs
  | otherwise = traceShow (length result) $ addKeyPads (number-1) keyPad result
  where
    result = getKeySequence keyPad inputs

makeKeypad :: String -> Keypad
makeKeypad lines = Keypad 'A' (buildOptionsMap grid)
  where
    grid = enumerateMultilineStringToVectorMap lines

complexity :: Int -> String -> Int
complexity keyPads code = numericPart * shortestButtonPressSequence keyPads code
  where
    numericPart = read $ filter isDigit code
    shortestSequence = shortestButtonPressSequence keyPads code

computeOptions :: Grid -> (Char, Char) -> [Inputs]
computeOptions grid (start, end)
 | badX == (currentPosition ^. _x) && badY == (yDelta + currentPosition ^. _y) = [xFirst] -- Prevent moving vertically to empty space
 | badY == (currentPosition ^. _y) && badX == (xDelta + currentPosition ^. _x) = [yFirst] -- Prevent moving horizontally to empty space
 | otherwise = nub [xFirst, yFirst]
  where
    (V2 badX badY) = locate '.' grid
    currentPosition = locate start grid
    desiredPosition = locate end grid
    xDelta = (desiredPosition ^. _x) - (currentPosition ^. _x)
    yDelta = (desiredPosition ^. _y) - (currentPosition ^. _y)
    xKeys = xDeltaToKeyPresses xDelta
    yKeys = yDeltaToKeyPresses yDelta
    xFirst = xKeys ++ yKeys ++ ['A']
    yFirst = yKeys ++ xKeys ++ ['A']

buildOptionsMap :: Grid -> M.Map (Char, Char) [Inputs]
buildOptionsMap grid = M.fromList $ map (\pair -> (pair, computeOptions grid pair)) pairs
  where
    keys = filter (/='.') $ M.elems grid
    pairs = concatMap (keyPairs keys) keys

keyPairs :: [Char] -> Char ->  [(Char, Char)]
keyPairs allKeys key = map (key,) allKeys

part1 :: [String] -> Int
part1 codes = sum $ map (complexity 2) codes

part2 :: [String] -> Int
part2 codes = shortestButtonPressSequence 7 "029A"
