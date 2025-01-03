module Solutions.Day20
  ( aoc20,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Common.Geometry
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set, elemAt, (\\))
import Linear (V2 (V2))
import Text.Trifecta (CharParsing (anyChar), Parser, many)

data RaceState = RaceState {visited :: M.Map Position Int, walls :: Set Position, end :: Position, time :: Int, position :: Position}
  deriving (Show, Eq, Ord)

aoc20 :: IO ()
aoc20 = do
  printSolutions 20 'A' $ MkAoCSolution parseInput part1
  printSolutions 20 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

completeRace :: Grid -> RaceState -> RaceState
completeRace g race
  | isComplete race = race
  | otherwise = completeRace g (drive g race)

isComplete :: RaceState -> Bool
isComplete (RaceState _ _ end _ position) = end == position

drive :: Grid -> RaceState -> RaceState
drive grid (RaceState visited walls end time position) = RaceState (M.insert position time visited) walls end (time + 1) neighbour
  where
    neighbour = elemAt 0 $ (M.keysSet (gridOrthogonalNeighbours grid position) \\ M.keysSet visited) \\ walls

race :: Grid -> RaceState
race grid = completeRace grid startingState
  where
    start = locate 'S' grid
    end = locate 'E' grid
    walls = M.keysSet $ M.filter (== '#') grid
    startingState = RaceState M.empty walls end 0 start

attemptCheat :: Int -> M.Map Position Int -> (Position, Int) -> [Int]
attemptCheat maxDepth timeFromPosition cheatPoint = concatMap (computeAllSavings timeFromPosition cheatPoint) [2 .. maxDepth]

computeAllSavings :: M.Map Position Int -> (Position, Int) -> Int -> [Int]
computeAllSavings positionTimings (startPosition, time) depth = map (timeToBeat -) usefulPoints
  where
    neighbours = neighboursWithDistance depth startPosition
    timeToBeat = time - depth
    usefulPoints = filter (< timeToBeat) $ mapMaybe (`M.lookup` positionTimings) neighbours

-- Compute all points with manhatten distance exactly N from a given point
neighboursWithDistance :: Int -> Point -> [Point]
neighboursWithDistance dist pt = concatMap (applyOffset pt) offsets
  where
    offsets = [0 .. dist - 1] `zip` reverse [1 .. dist]

applyOffset :: Point -> (Int, Int) -> [Point]
applyOffset (V2 x y) (offset, inverseOffset) =
  [ V2 (x + offset) (y + inverseOffset),
    V2 (x + inverseOffset) (y - offset),
    V2 (x - offset) (y - inverseOffset),
    V2 (x - inverseOffset) (y + offset)
  ]

computeSavings :: Int -> Grid -> [Int]
computeSavings cheatDepth grid = concatMap (attemptCheat cheatDepth positionTimings) cheatLocations
  where
    raceState = race grid
    totalTime = time raceState
    positionTimings = M.insert (end raceState) 0 $ M.map (totalTime -) $ visited raceState
    cheatLocations = M.toList positionTimings

part1 :: Grid -> Int
part1 = length . filter (>= 100) . computeSavings 2

part2 :: Grid -> Int
part2 = length . filter (>= 100) . computeSavings 20
