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
import Data.Set (Set, elemAt, (\\))
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.List
import Common.ListUtils (associateBy)

data RaceState = RaceState{visited :: M.Map Position Int, walls :: Set Position, end :: Position, time :: Int, position :: Position}
  deriving (Show, Eq, Ord)

aoc20 :: IO ()
aoc20 = do
  printSolutions 20 $ MkAoCSolution parseInput part1
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


attemptCheat :: Grid -> M.Map Position Int -> (Position, Int) -> [Int]
attemptCheat grid raceResults (startPosition, time) = mapMaybe (scoreCheat raceResults (time+1)) neighbours ++ mapMaybe (scoreCheat raceResults (time+2)) neighbours2
  where
    neighbours = M.keys $ M.filter (=='#') $ gridOrthogonalNeighbours grid startPosition
    neighboursOfNeighbours = Data.List.concatMap (M.keys . gridOrthogonalNeighbours grid) neighbours
    neighbours2 = [neighbour | neighbour <- neighboursOfNeighbours, neighbour `notElem` neighbours, neighbour /= startPosition]

scoreCheat :: M.Map Position Int -> Int -> Position -> Maybe Int
scoreCheat timeFromPosition time position = if isJust endTime then Just (time + fromJust endTime) else Nothing
  where
    endTime = M.lookup position timeFromPosition

part1 :: Grid -> Int
part1 grid = length $ filter (>=100) savings
  where
    raceState = race grid
    totalTime = time raceState
    timeToPosition = M.insert (end raceState) totalTime $ visited raceState
    timeFromPosition = M.map (totalTime -) timeToPosition
    cheatLocations = M.toList timeToPosition
    savings = Data.List.map (totalTime -) $ concatMap (filter (< totalTime) . attemptCheat grid timeFromPosition) cheatLocations

part2 :: Grid -> String
part2 = undefined
