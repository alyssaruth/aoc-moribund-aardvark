module Solutions.Day22 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Data.Bits (xor)
import Text.Trifecta (Parser, anyChar, integer, many, some, token)
import qualified Data.Map as M
import Common.ListUtils (window2, windowN)
import Control.Lens (imap)
import Debug.Trace (traceShow)

aoc22 :: IO ()
aoc22 = do
  printSolutions 22 'A' $ MkAoCSolution parseInput part1
  printSolutions 22 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = some $ token integer

part1 :: [Integer] -> Int
part1 = sum . map (getNthEvolution 2000 . fromInteger)

getNthEvolution :: Int -> Int -> Int
getNthEvolution n num
  | n == 0 = num
  | otherwise = getNthEvolution (n - 1) (evolveSecretNumber num)

evolveSecretNumber :: Int -> Int
evolveSecretNumber num = mixAndPrune stepTwo (stepTwo * 2048)
  where
    stepOne = mixAndPrune num (num * 64)
    stepTwo = mixAndPrune stepOne (stepOne `div` 32)

mixAndPrune :: Int -> Int -> Int
mixAndPrune secretNumber newValue = (secretNumber `xor` newValue) `mod` 16777216

part2 :: [Integer] -> Int
part2 = maximum . M.elems . foldl updateDeltaMap M.empty

updateDeltaMap :: M.Map [Int] Int -> Integer -> M.Map [Int] Int
updateDeltaMap mapSoFar secretNumber = M.unionWith (+) mapSoFar newMap
  where
    newMap = makeDeltaMap 2000 (fromInteger secretNumber)

makeDeltaMap :: Int -> Int -> M.Map [Int] Int
makeDeltaMap n secretNumber = M.fromList $ reverse $ imap (toPair priceSequence) (windowN 4 differences)
  where
    priceSequence = makePriceSequence n secretNumber
    differences = map (uncurry (flip (-))) $ window2 priceSequence

toPair :: [Int] -> Int -> [Int] -> ([Int], Int)
toPair priceSequence ix diffSequence = (diffSequence, priceSequence!!(ix+4))

makePriceSequence :: Int -> Int -> [Int]
makePriceSequence n secretNumber = priceSequence (n - 1) secretNumber [secretNumber `mod` 10]

priceSequence :: Int -> Int -> [Int] -> [Int]
priceSequence n secretNumber pricesSoFar
  | n == 0 = pricesSoFar
  | otherwise = priceSequence (n - 1) next (pricesSoFar ++ [nextDigit])
  where
    next = evolveSecretNumber secretNumber
    nextDigit = next `mod` 10
