module Solutions.Day1
  ( aoc1practice
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.Char           (isDigit)
import           Text.Trifecta       (Parser, TokenParsing (token), alphaNum,
                                      some)

aoc1practice :: IO ()
aoc1practice = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

type CalibrationLines = [String]

parseInput :: Parser CalibrationLines
parseInput = do
  some $ token $ some alphaNum

part1 :: CalibrationLines -> Int
part1 = sum . map calibrationValue

part2 :: CalibrationLines -> Int
part2 = sum . map (calibrationValue . validNumbers)

calibrationValue :: String -> Int
calibrationValue x = read [firstNumber x, lastNumber x]

firstNumber :: String -> Char
firstNumber = head . filter isDigit

lastNumber :: String -> Char
lastNumber = firstNumber . reverse

validNumbers :: String -> String
validNumbers [] = ""
validNumbers l@(x:xs)
    | isDigit x = [x] ++ validNumbers xs
    | otherwise = (parseStringNum l) ++ (validNumbers xs)

parseStringNum :: String -> String
parseStringNum ('o':'n':'e':things) = ['1']
parseStringNum ('t':'w':'o':stuff) = ['2']
parseStringNum ('t':'h':'r':'e':'e':rubbish) = ['3']
parseStringNum ('f':'o':'u':'r':remainder) = ['4']
parseStringNum ('f':'i':'v':'e':gubbins) = ['5']
parseStringNum ('s':'i':'x':doodad) = ['6']
parseStringNum ('s':'e':'v':'e':'n':mcguffin) = ['7']
parseStringNum ('e':'i':'g':'h':'t':junk) = ['8']
parseStringNum ('n':'i':'n':'e':remnants) = ['9']
parseStringNum x = []

