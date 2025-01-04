module Solutions.Day25
  ( aoc25,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Common.Geometry
import Data.List (partition)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Linear (V2 (..))
import Text.Trifecta (Parser, anyChar, many)
import qualified Data.Set as S

type Key = S.Set Point

type Lock = S.Set Point

aoc25 :: IO ()
aoc25 = do
  printSolutions 25 'A' $ MkAoCSolution parseInput part1
  printSolutions 25 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser ([Key], [Lock])
parseInput = do
  allChars <- many anyChar
  let grids = map enumerateMultilineStringToVectorMap $ splitOn "\n\n" allChars
  pure $ partition isKey $ map (M.keysSet . M.filter (=='#')) grids

part1 :: ([Key], [Lock]) -> Int
part1 (keys, locks) = length [key | key <- keys, lock <- locks, fits key lock]

isKey :: S.Set Point -> Bool
isKey = any (\(V2 x y) -> y == 0)

fits :: Key -> Lock -> Bool
fits key lock = S.null $ key `S.intersection` lock

part2 :: ([Key], [Lock]) -> String
part2 = const "Merry Christmas!"
