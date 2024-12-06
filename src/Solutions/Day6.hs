module Solutions.Day6
  ( aoc6,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry
import Control.Lens ((^.))
import Data.List as L
import qualified Data.Map as M
import Data.Set as S
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Trifecta (Parser, anyChar, many)

type Direction = Point

type Position = Point

data Guard = Guard {position :: Position, direction :: Direction}
  deriving (Show, Eq, Ord)

data PatrolResult = PatrolResult {history :: Set Guard, looped :: Bool}

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

makeAllMoves :: Grid -> Set Guard -> Guard -> PatrolResult
makeAllMoves g guards guard
  | newTile == ' ' = PatrolResult guards False
  | newGuard `member` guards = PatrolResult guards True
  | otherwise = makeAllMoves g (S.insert newGuard guards) newGuard
  where
    newGuard = makeMove g guard
    newTile = M.findWithDefault ' ' (position newGuard) g

makeMove :: Grid -> Guard -> Guard
makeMove grid guard = if obstacle == '#' then Guard (position guard) (rotate $ direction guard) else Guard newPosition (direction guard)
  where
    newPosition = position guard + direction guard
    obstacle = M.findWithDefault ' ' newPosition grid

rotate :: Direction -> Direction
rotate d = V2 (-(d ^. _y)) (d ^. _x)

findGuard :: Grid -> Position
findGuard = head . M.keys . M.filter (== '^')

processGrid :: Grid -> PatrolResult
processGrid g = makeAllMoves g (S.singleton initialGuard) initialGuard
  where
    initialGuard = Guard (findGuard g) (V2 0 (-1))

part1 :: Grid -> Int
part1 = length . S.map position . history . processGrid

generateNewGrids :: Grid -> [Grid]
generateNewGrids g = L.map (addObstacle g) positions
  where
    positions = L.delete (findGuard g) $ S.toList $ S.map position $ history $ processGrid g

addObstacle :: Grid -> Position -> Grid
addObstacle g p = M.insert p '#' g

part2 :: Grid -> Int
part2 = length . L.filter (looped . processGrid) . generateNewGrids
