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
import Common.ListUtils (window2)

type Inputs = [Char]
data Keypad = Keypad {arm :: Char, optionsMap :: M.Map (Char, Char) Inputs}

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
moveArm keyPad nextKey inputs = inputs ++ optionsMap keyPad M.! (arm keyPad, nextKey)


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

-- For every possible key pair on a keyPad, compute a single sequence of keypresses on a directional keypad that's guaranteed to be optimal.
-- Changing direction is expensive as it requires the robot above us to move the arm. So we can immediately disregard sequences like ^>^.
-- This means we only ever have at *most* 2 options - "xFirst" >^^A or "yFirst" ^^>A. To pick between these we do various things:
--   - Some must be discounted due to the "panic" rule - one of the two paths would make a robot hover over empty space
--   - Sometimes we only have one option anyway because it's a straight line, e.g. (2, 8) -> ^^A
--   - Otherwise, things are not so obvious. But considering the layout of the directional keypad, we can still pick optimally:
--           +---+---+
--           | ^ | A |
--       +---+---+---+
--       | < | v | > |
--       +---+---+---+
--   - < is the most expensive direction - it is furthest from A. 
--   - We're always going to return to A at the end, so if < is required we should do those first. 
--   - If our horizontal component is >, then we should do the vertical part first.
-- I haven't actually thought much about the numeric keypad case, but this seems to work there too for... some reason
computeOption :: Grid -> (Char, Char) -> Inputs
computeOption grid (start, end)
 | badX == (currentPosition ^. _x) && badY == (yDelta + currentPosition ^. _y) = xFirst -- Prevent moving vertically to empty space
 | badY == (currentPosition ^. _y) && badX == (xDelta + currentPosition ^. _x) = yFirst -- Prevent moving horizontally to empty space
 | xFirst == yFirst = xFirst -- Only one actual option, i.e. straight line
 | head xKeys == '<' = xFirst
 | otherwise = yFirst
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

xDeltaToKeyPresses :: Int -> Inputs
xDeltaToKeyPresses delta = replicate (abs delta) key
  where
    key = if delta < 0 then '<' else '>'

yDeltaToKeyPresses :: Int -> Inputs
yDeltaToKeyPresses delta = replicate (abs delta) key
  where
    key = if delta < 0 then '^' else 'v'

buildOptionsMap :: Grid -> M.Map (Char, Char) Inputs
buildOptionsMap grid = M.fromList $ map (\pair -> (pair, computeOption grid pair)) pairs
  where
    keys = filter (/='.') $ M.elems grid
    pairs = concatMap (keyPairs keys) keys

keyPairs :: [Char] -> Char ->  [(Char, Char)]
keyPairs allKeys key = map (key,) allKeys

part1 :: [String] -> Int
part1 codes = sum $ map (complexity 2) codes

part2 :: [String] -> Int
part2 codes = shortestButtonPressSequence 10 "029A"
