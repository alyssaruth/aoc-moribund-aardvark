module Solutions.Day22 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Data.Bits (xor, Bits (shiftL, shiftR))
import Text.Trifecta (Parser, anyChar, integer, many, some, token)
import qualified Data.Map as M
import Common.ListUtils (window2, windowN)
import Control.Lens (imap)
import Debug.Trace (traceShow)
import Data.List (sort)

pruneBase = 2^24

aoc22 :: IO ()
aoc22 = do
  printSolutions 22 'A' $ MkAoCSolution parseInput part1
  printSolutions 22 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = some $ token integer

part1 :: [Integer] -> Int
part1 = sum . map (getNthEvolution 2000 . fromInteger)

getNthEvolution :: Int -> Int -> Int
getNthEvolution n num = iterate evolveSecretNumber num !! n

evolveSecretNumber :: Int -> Int
evolveSecretNumber = stepThree . stepTwo . stepOne
  where
    stepOne x = mixAndPrune x (x `shiftL` 6)
    stepTwo x = mix x (x `shiftR` 5) -- Prune unnecessary as the number is getting smaller
    stepThree x = mixAndPrune x (x `shiftL` 11)

mixAndPrune :: Int -> Int -> Int
mixAndPrune secretNumber newValue = mix secretNumber newValue `mod` pruneBase

mix :: Int -> Int -> Int
mix secretNumber newValue = secretNumber `xor` newValue

part2 :: [Integer] -> Int
part2 = maximum . M.elems . foldl updateDeltaMap M.empty

updateDeltaMap :: M.Map [Int] Int -> Integer -> M.Map [Int] Int
updateDeltaMap mapSoFar secretNumber = unioned
  where
    newMap = makeDeltaMap 2000 (fromInteger secretNumber)
    unioned = M.unionWith (+) mapSoFar newMap

makeDeltaMap :: Int -> Int -> M.Map [Int] Int
makeDeltaMap n secretNumber = M.fromList $ reverse zipped
  where
    priceSequence = makePriceSequence n secretNumber
    differences = map (uncurry (flip (-))) $ window2 priceSequence
    zipped = zip (windowN 4 differences) (drop 4 priceSequence)

makePriceSequence :: Int -> Int -> [Int]
makePriceSequence n secretNumber = priceSequence (n - 1) secretNumber [secretNumber `mod` 10]

priceSequence :: Int -> Int -> [Int] -> [Int]
priceSequence n secretNumber pricesSoFar
  | n == 0 = pricesSoFar
  | otherwise = priceSequence (n - 1) next (pricesSoFar ++ [nextDigit])
  where
    next = evolveSecretNumber secretNumber
    nextDigit = next `mod` 10
