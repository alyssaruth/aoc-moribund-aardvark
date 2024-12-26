module Solutions.Day20
  ( aoc20,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Common.Geometry
import Text.Trifecta (CharParsing (anyChar), Parser, many)
import Data.Set (Set, elemAt, (\\), notMember, union, fromList)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.List
import Common.ListUtils (associateBy)
import Data.List (nub)
import Linear (V2(V2))

data RaceState = RaceState{visited :: M.Map Position Int, walls :: Set Position, end :: Position, time :: Int, position :: Position}
  deriving (Show, Eq, Ord)

data PartialCheat = PartialCheat{steps :: Int, points :: [Position]}

aoc20 :: IO ()
aoc20 = do
  printTestSolutions 20 $ MkAoCSolution parseInput part1
  printSolutions 20 $ MkAoCSolution parseInput part2

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
    walls = M.keysSet $ M.filter (=='#') grid
    startingState = RaceState M.empty walls end 0 start

attemptCheat :: Grid -> Int -> M.Map Position Int -> (Position, Int) -> [Int]
attemptCheat grid maxDepth timeFromPosition (startPosition, time) = concatMap (cheatScores timeFromPosition time) partialCheats
  where
    pathPoints = associateBy (`distance` startPosition) $ M.keys timeFromPosition
    distances = [2..maxDepth]
    partialCheats = map (toPartialCheat startPosition pathPoints) distances

toPartialCheat :: Position -> M.Map Int [Point] -> Int -> PartialCheat
toPartialCheat position pathPoints depth = PartialCheat depth validPoints
  where
    validPoints = M.findWithDefault [] depth pathPoints

cheatScores :: M.Map Position Int -> Int -> PartialCheat -> [Int]
cheatScores timeFromPosition time (PartialCheat steps points) = map ((+steps) . (+time)) usefulPoints
  where
    usefulPoints = mapMaybe (`M.lookup` timeFromPosition) points

distance :: Point -> Point -> Int
distance (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

computeSavings :: Int -> Grid -> [Int]
computeSavings cheatDepth grid = Data.List.map (totalTime -) $ concatMap (filter (< totalTime) . attemptCheat grid cheatDepth timeFromPosition) cheatLocations
  where
    raceState = race grid
    totalTime = time raceState
    timeToPosition = M.insert (end raceState) totalTime $ visited raceState
    timeFromPosition = M.map (totalTime -) timeToPosition
    cheatLocations = M.toList timeToPosition

part1 :: Grid -> Int
part1 = length . filter (>=100) . computeSavings 2

part2 :: Grid -> Int
part2 = length . filter (>=100) . computeSavings 20
