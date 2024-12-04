module Solutions.Day4
  ( aoc4,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry
import Control.Lens ((^.))
import Data.Map (keys)
import qualified Data.Map as M
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta (Parser)

type Line = [Point]

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1
  printSolutions 4 $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

part1 :: Grid -> Int
part1 g = length $ filter (== "XMAS") $ map (toWord g) (allPotentialLines g)

toWord :: Grid -> Line -> String
toWord = map . flip (M.findWithDefault ' ')

allPotentialLines :: Grid -> [Line]
allPotentialLines g = concatMap expandLine $ keys g

expandLine :: Point -> [Line]
expandLine x = [[x, n, n + (n - x), n + 2 * (n - x)] | n <- neighboursL x]

part2 :: Grid -> Int
part2 = length . filter validSubGrid . subGridsOfA

validSubGrid :: Grid -> Bool
validSubGrid g = toWord g (corners g) `elem` ["MSMS", "SMSM", "MMSS", "SSMM"]

subGridsOfA :: Grid -> [Grid]
subGridsOfA g = filter ((== 9) . length) $ map (gridNeighboursInclusive g) $ keys $ M.filter (== 'A') g

-- TL, TR, BL, BR
corners :: Grid -> [Point]
corners g = [V2 xMin yMin, V2 xMax yMin, V2 xMin yMax, V2 xMax yMax]
  where
    xMin = minimum $ map (^. _x) $ M.keys g
    xMax = maximum $ map (^. _x) $ M.keys g
    yMin = minimum $ map (^. _y) $ M.keys g
    yMax = maximum $ map (^. _y) $ M.keys g