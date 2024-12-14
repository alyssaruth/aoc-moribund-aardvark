module Solutions.Day11
  ( aoc11,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.ListUtils (associateBy)
import Data.List (genericLength)
import qualified Data.Map as M
import Text.Trifecta (CharParsing (char), Parser, integer', many, sepBy)

type Stones = M.Map Integer Integer

aoc11 :: IO ()
aoc11 = do
  printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = sepBy integer' (char ' ')

applyRule :: (Integer, Integer) -> [(Integer, Integer)]
applyRule (stone, count)
  | stone == 0 = [(1, count)]
  | even $ length $ show stone = [(x, count) | x <- splitStone stone]
  | otherwise = [(stone * 2024, count)]

splitStone :: Integer -> [Integer]
splitStone x = [read $ take size digits, read $ drop size digits]
  where
    digits = show x
    size = length digits `div` 2

iterateMap :: Stones -> Stones
iterateMap stoneMap = M.fromListWith (+) $ concatMap applyRule $ M.toList stoneMap

blinkNTimes :: Integer -> Stones -> Stones
blinkNTimes 0 m = m
blinkNTimes n m = blinkNTimes (n - 1) $ iterateMap m

parseInitialState :: [Integer] -> Stones
parseInitialState = M.map genericLength . associateBy id

part1 :: [Integer] -> Integer
part1 = sum . blinkNTimes 25 . parseInitialState

part2 :: [Integer] -> Integer
part2 = sum . blinkNTimes 75 . parseInitialState
