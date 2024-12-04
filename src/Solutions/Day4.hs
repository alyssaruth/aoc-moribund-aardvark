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
part1 g = length $ filter (=="XMAS") $ map (toWord g) (wordies g)

toWord :: Grid Char -> Line -> String
toWord g = map (flip (M.findWithDefault ' ') g)

wordies :: Grid Char -> [Line]
wordies g = concatMap wordsFromPoint $ keys g

-- Unnecessary optimisation
-- startingPoints :: Grid Char -> [Point]
-- startingPoints g = keys (M.filter (=='X') g)

wordsFromPoint :: Point -> [Line]
wordsFromPoint x = expandLines [[x]]

expandLines :: [Line] -> [Line]
expandLines = concatMap expandLine

expandLine :: Line -> [Line]
expandLine [x] = expandLines [[x, n] | n <- neighboursL x]
expandLine [x, y] = expandLines [[x, y, y + (y - x)]]
expandLine [x, y, z] = [[x, y, z, z + (z - y)]]

part2 :: Grid Char -> String
part2 = undefined
