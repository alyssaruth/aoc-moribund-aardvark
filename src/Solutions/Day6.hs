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
import Data.List
import qualified Data.Map as M
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Trifecta (Parser, anyChar, many)

type Direction = Point

type Position = Point

data Guard = Guard {position :: Position, direction :: Direction}
  deriving (Show)

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

makeAllMoves :: Grid -> [Guard] -> [Guard]
makeAllMoves g guards = if newTile == ' ' then guards else makeAllMoves g (guards ++ [newGuard])
  where
    newGuard = makeMove g (last guards)
    newTile = M.findWithDefault ' ' (position newGuard) g

makeMove :: Grid -> Guard -> Guard
makeMove grid guard = if obstacle == '#' then Guard (position guard) (rotate $ direction guard) else Guard newPosition (direction guard)
  where
    newPosition = position guard + direction guard
    obstacle = M.findWithDefault ' ' newPosition grid

rotate :: Direction -> Direction
rotate d = V2 (-(d ^. _y)) (d ^. _x)

findGuard :: Grid -> Position
findGuard g = head $ M.keys $ M.filter (== '^') g

part1 :: Grid -> Int
part1 g = length $ nub $ map position $ makeAllMoves g [initialGuard]
  where
    initialGuard = Guard (findGuard g) (V2 0 (-1))

part2 :: Grid -> String
part2 = undefined
