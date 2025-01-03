module Solutions.Day22 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Common.ListUtils (window2, windowN)
import Data.Bits (Bits (shiftL, shiftR), xor)
import Data.Hashable (hash)
import qualified Data.Map as M
import Text.Trifecta (Parser, anyChar, integer, some, token)

pruneBase = 2 ^ 24

aoc22 :: IO ()
aoc22 = do
  printSolutions 22 'A' $ MkAoCSolution parseInput part1
  printSolutions 22 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = some $ token integer

part1 :: [Integer] -> Integer
part1 = sum . map (getNthEvolution 2000)

getNthEvolution :: Int -> Integer -> Integer
getNthEvolution n num = iterate evolveSecretNumber num !! n

evolveSecretNumber :: Integer -> Integer
evolveSecretNumber = stepThree . stepTwo . stepOne
  where
    stepOne x = mixAndPrune x (x `shiftL` 6)
    stepTwo x = mix x (x `shiftR` 5) -- Prune unnecessary as the number is getting smaller
    stepThree x = mixAndPrune x (x `shiftL` 11)

mixAndPrune :: Integer -> Integer -> Integer
mixAndPrune secretNumber newValue = mix secretNumber newValue `mod` pruneBase

mix :: Integer -> Integer -> Integer
mix secretNumber newValue = secretNumber `xor` newValue

part2 :: [Integer] -> Integer
part2 = maximum . M.elems . foldl updateDeltaMap M.empty

updateDeltaMap :: M.Map Int Integer -> Integer -> M.Map Int Integer
updateDeltaMap mapSoFar secretNumber = M.unionWith (+) mapSoFar newMap
  where
    newMap = makeDeltaMap 2000 (fromInteger secretNumber)

makeDeltaMap :: Int -> Integer -> M.Map Int Integer
makeDeltaMap n secretNumber = M.fromList $ reverse zipped
  where
    priceSequence = makePriceSequence n secretNumber
    differences = map (uncurry (flip (-))) $ window2 priceSequence
    zipped = zip (map hash (windowN 4 differences)) (drop 4 priceSequence)

makePriceSequence :: Int -> Integer -> [Integer]
makePriceSequence n secretNumber = priceSequence (n - 1) secretNumber [secretNumber `mod` 10]

priceSequence :: Int -> Integer -> [Integer] -> [Integer]
priceSequence n secretNumber pricesSoFar
  | n == 0 = pricesSoFar
  | otherwise = priceSequence (n - 1) next (pricesSoFar ++ [nextDigit])
  where
    next = evolveSecretNumber secretNumber
    nextDigit = next `mod` 10
