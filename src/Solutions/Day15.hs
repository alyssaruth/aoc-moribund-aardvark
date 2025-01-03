module Solutions.Day15 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Common.Geometry
import Control.Lens ((^.))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Trifecta (CharParsing (anyChar, string), Parser, Parsing (try), TokenParsing (token), manyTill, oneOf, some)

type Box = Set Point

data Warehouse = Warehouse {robot :: Point, boxes :: Set Box, walls :: Set Point}

data MoveResult = MoveResult {affectedBoxes :: Set Box, hitWall :: Bool}

aoc15 :: IO ()
aoc15 = do
  printSolutions 15 'A' $ MkAoCSolution parseInput part1
  printSolutions 15 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid, [Point])
parseInput = do
  gridLines <- manyTill anyChar (try (string "\n\n"))
  moves <- some $ token $ oneOf "<>^v"
  pure (enumerateMultilineStringToVectorMap gridLines, map parseMove moves)

parseMove :: Char -> Point
parseMove m
  | m == 'v' = V2 0 1
  | m == '^' = V2 0 (-1)
  | m == '>' = V2 1 0
  | m == '<' = V2 (-1) 0

factoryWarehouse :: Grid -> Warehouse
factoryWarehouse g = Warehouse (locate '@' g) (findBoxes g) (M.keysSet $ M.filter (== '#') g)

findBoxes :: Grid -> Set Box
findBoxes = S.map S.singleton . M.keysSet . M.filter (== 'O')

findLongBoxes :: Grid -> Set Box
findLongBoxes = S.map (\x -> S.fromList [x, x + V2 1 0]) . M.keysSet . M.filter (== '[')

scoreWarehouse :: Warehouse -> Int
scoreWarehouse = sum . S.map gpsCoordinates . boxes

gpsCoordinates :: Box -> Int
gpsCoordinates box = x + 100 * y
  where
    x = minimum $ S.map (^. _x) box
    y = minimum $ S.map (^. _y) box

makeMove :: Warehouse -> Point -> Warehouse
makeMove warehouse direction = updateWarehouse warehouse direction result
  where
    result = exploreMove warehouse direction (S.singleton (robot warehouse)) S.empty

exploreMove :: Warehouse -> Point -> Set Point -> Set Box -> MoveResult
exploreMove warehouse direction currentPoints currentBoxes
  | hitWall = MoveResult S.empty True
  | null newBoxes = MoveResult currentBoxes False
  | otherwise = exploreMove warehouse direction newPoints (currentBoxes `S.union` newBoxes)
  where
    potentialNewPoints = S.map (+ direction) currentPoints
    hitWall = not $ null $ walls warehouse `S.intersection` potentialNewPoints
    potentialNewBoxes = S.filter (\box -> not $ null $ box `S.intersection` potentialNewPoints) (boxes warehouse)
    newBoxes = potentialNewBoxes `S.difference` currentBoxes
    newPoints = S.unions newBoxes

updateWarehouse :: Warehouse -> Point -> MoveResult -> Warehouse
updateWarehouse (Warehouse robot boxes walls) direction (MoveResult affectedBoxes hitWall)
  | hitWall = Warehouse robot boxes walls
  | otherwise = Warehouse (robot + direction) (shiftedBoxes `S.union` unaffectedBoxes) walls
  where
    unaffectedBoxes = boxes `S.difference` affectedBoxes
    shiftedBoxes = S.map (S.map (+ direction)) affectedBoxes

part1 :: (Grid, [Point]) -> Int
part1 (g, moves) = scoreWarehouse $ foldl makeMove (factoryWarehouse g) moves

factorySecondWarehouse :: Grid -> Warehouse
factorySecondWarehouse g = Warehouse (locate '@' widerMap) (findLongBoxes widerMap) (M.keysSet $ M.filter (== '#') widerMap)
  where
    widerMap = doubleUp g

doubleUp :: Grid -> Grid
doubleUp = enumerateMultilineStringToVectorMap . concatMap doubleUpChar . renderVectorMap

doubleUpChar :: Char -> [Char]
doubleUpChar '#' = "##"
doubleUpChar 'O' = "[]"
doubleUpChar '.' = ".."
doubleUpChar '@' = "@."
doubleUpChar '\n' = ['\n']

part2 :: (Grid, [Point]) -> Int
part2 (g, moves) = scoreWarehouse $ foldl makeMove (factorySecondWarehouse g) moves
