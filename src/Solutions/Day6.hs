module Solutions.Day6
  ( aoc6,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Common.Geometry
import Control.Lens ((^.))
import Data.List as L
import qualified Data.Map as M
import Data.Set as S
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Trifecta (Parser, anyChar, many)
import Debug.Trace (traceShow)


data Guard = Guard {position :: Position, direction :: Direction}
  deriving (Show, Eq, Ord)

data PatrolResult = PatrolResult {history :: Set Guard, looped :: Bool}
  deriving (Show)

type MovementFn = Grid -> Guard -> Guard

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 'A' $ MkAoCSolution parseInput part1
  printSolutions 6 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

makeAllMoves :: MovementFn -> Grid -> Set Guard -> Guard -> PatrolResult
makeAllMoves move g guards guard
  | newTile == ' ' = PatrolResult guards False
  | newGuard `member` guards = PatrolResult guards True
  | otherwise = makeAllMoves move g (S.insert newGuard guards) newGuard
  where
    newGuard = move g guard
    newTile = lookupTile g (position newGuard)

makeSingleMove :: Grid -> Guard -> Guard
makeSingleMove grid guard 
  | lookupTile grid newPosition == '#' = Guard (position guard) (rotate $ direction guard) 
  | otherwise = Guard newPosition (direction guard)
  where
    newPosition = position guard + direction guard

-- For part 2 we don't need to track the full history, so we "leap" from obstacle to obstacle rather than taking one step at a time
moveToNextObstacle :: Grid -> Guard -> Guard
moveToNextObstacle grid (Guard position direction)
  | lookupTile grid nextObstacle == '#' = Guard (nextObstacle - direction) (rotate direction) 
  | otherwise = Guard nextObstacle direction
  where
    pathInFront = iterate (+ direction) (position+direction)
    nextObstacle = head $ L.filter ((/= '.') . lookupTile grid) pathInFront

lookupTile :: Grid -> Position -> Char
lookupTile grid position = M.findWithDefault ' ' position grid

rotate :: Direction -> Direction
rotate d = V2 (-(d ^. _y)) (d ^. _x)

findGuard :: Grid -> Position
findGuard = head . M.keys . M.filter (== '^')

processGrid :: MovementFn -> Grid -> PatrolResult
processGrid moveFn g = makeAllMoves moveFn g (S.singleton initialGuard) initialGuard
  where
    initialGuard = Guard (findGuard g) (V2 0 (-1))

part1 :: Grid -> Int
part1 = length . S.map position . history . processGrid makeSingleMove

generateNewGrids :: Grid -> [Grid]
generateNewGrids g = L.map (addObstacle g) positions
  where
    positions = L.delete (findGuard g) $ S.toList $ S.map position $ history $ processGrid makeSingleMove g

addObstacle :: Grid -> Position -> Grid
addObstacle g p = M.insert p '#' g

part2 :: Grid -> Int
part2 = length . L.filter (looped . processGrid moveToNextObstacle) . generateNewGrids