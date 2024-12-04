module Solutions.Day4
  ( aoc4
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser)
import Common.Geometry
import Text.Parser.Combinators
import Text.Parser.Char
import Data.Map (keys)
import qualified Data.Map as M

type Line = [Point]

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1
  printSolutions 4 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

part1 :: Grid Char -> Int
part1 g = length $ filter (=="XMAS") $ map (toWord g) (allPotentialLines g)

toWord :: Grid Char -> Line -> String
toWord = map . flip (M.findWithDefault ' ')

allPotentialLines :: Grid Char -> [Line]
allPotentialLines g = concatMap expandLine $ keys g

expandLine :: Point -> [Line]
expandLine x = [[x, n, n + (n-x), n + 2*(n-x)] | n <- neighboursL x]

part2 :: Grid Char -> String
part2 = undefined
