module Solutions.Day10
  ( aoc10,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry
import Data.Char (digitToInt, intToDigit)
import Data.List (nub, singleton)
import qualified Data.Map as M
import Text.Trifecta (CharParsing (anyChar), Parser, many)

type Path = [Point]

aoc10 :: IO ()
aoc10 = do
  printSolutions 10 $ MkAoCSolution parseInput part1
  printSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

trailheads :: Grid -> [Point]
trailheads = M.keys . M.filter (== '0')

findAllPaths :: Grid -> Point -> [Path]
findAllPaths g p = findAllPathsR g [[p]]

findAllPathsR :: Grid -> [Path] -> [Path]
findAllPathsR g paths
  | length (last paths) == 10 = paths
  | otherwise = findAllPathsR g $ concatMap (iteratePath g) paths

iteratePath :: Grid -> Path -> [Path]
iteratePath g p = map (p ++) validNeighbours
  where
    currentPosition = last p
    currentElevation = digitToInt $ M.findWithDefault '0' currentPosition g
    validNeighbours = map singleton $ M.keys $ M.filter (== intToDigit (currentElevation + 1)) $ gridOrthogonalNeighbours g currentPosition

scoreTrailhead :: Grid -> Point -> Int
scoreTrailhead g p = length $ nub $ map last $ findAllPaths g p

rateTrailhead :: Grid -> Point -> Int
rateTrailhead g p = length $ nub $ findAllPaths g p

part1 :: Grid -> Int
part1 g = sum $ map (scoreTrailhead g) $ trailheads g

part2 :: Grid -> Int
part2 g = sum $ map (rateTrailhead g) $ trailheads g
