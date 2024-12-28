module Solutions.Day22 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, anyChar, many, some, token, integer)
import Data.Bits (xor)

aoc22 :: IO ()
aoc22 = do
  printSolutions 22 $ MkAoCSolution parseInput part1
  printSolutions 22 $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = some $ token integer

part1 :: [Integer] -> Int
part1 = sum . map (getNthEvolution 2000 . fromInteger)

part2 :: [Integer] -> [Integer]
part2 = undefined

getNthEvolution :: Int -> Int -> Int
getNthEvolution n num 
  | n == 0 = num
  | otherwise = getNthEvolution (n-1) (evolveSecretNumber num)

evolveSecretNumber :: Int -> Int
evolveSecretNumber num = mixAndPrune stepTwo (stepTwo * 2048)
  where
    stepOne = mixAndPrune num (num*64)
    stepTwo = mixAndPrune stepOne (stepOne `div` 32)


mixAndPrune :: Int -> Int -> Int
mixAndPrune secretNumber newValue = (secretNumber `xor` newValue) `mod` 16777216
