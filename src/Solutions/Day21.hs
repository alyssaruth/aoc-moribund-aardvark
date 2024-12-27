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

type Inputs = [Char]
data Keypad = Keypad {layout :: Grid, arm :: Position}


codeEntryCombos :: Keypad -> Inputs -> [Inputs]
codeEntryCombos keyPad keys = enterCode keyPad keys [""]

enterCode :: Keypad -> Inputs -> [Inputs] -> [Inputs]
enterCode keyPad keys inputCombos
 | keys == "" = inputCombos
 | otherwise = enterCode newKeyPad (tail keys) $ concatMap (appendInputs nextInputs) inputCombos
  where
    nextKey = head keys
    (newKeyPad, nextInputs) = moveArm keyPad nextKey

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
    newKeyPad = Keypad (layout keyPad) desiredPosition

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

part1 :: [String] -> Int
part1 codes = sum $ map complexity codes

shortestButtonPressSequence :: String -> Int
shortestButtonPressSequence code = minimum $ map length $ addKeyPad directionalKeyPad $ addKeyPad directionalKeyPad numericCombos
  where
    numericKeyPad = makeKeypad "789\n456\n123\n.0A"
    directionalKeyPad = makeKeypad ".^A\n<v>"
    numericCombos = codeEntryCombos numericKeyPad code

makeKeypad :: String -> Keypad
makeKeypad lines = Keypad grid (locate 'A' grid)
  where
    grid = enumerateMultilineStringToVectorMap lines

complexity :: String -> Int
complexity code = traceShow (show shortestSequence ++ " * " ++ show numericPart) $ numericPart * shortestButtonPressSequence code
  where
    numericPart = read $ filter isDigit code
    shortestSequence = shortestButtonPressSequence code

addKeyPad :: Keypad -> [Inputs] -> [Inputs]
addKeyPad keyPad = concatMap (codeEntryCombos keyPad)

part2 :: [String] -> String
part2 = undefined
