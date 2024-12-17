module Solutions.Day15 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry
import Control.Lens ((^.))
import Data.List (intersect, minimumBy, (\\), singleton)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Trifecta (CharParsing (anyChar, string), Parser, Parsing (try), TokenParsing (token), manyTill, oneOf, some)

type Box = [Point]

data Warehouse = Warehouse {robot :: Point, boxes :: [Box], walls :: Set Point}

data MoveResult = MoveResult {affectedBoxes :: [Box], hitWall :: Bool}

aoc15 :: IO ()
aoc15 = do
  printSolutions 15 $ MkAoCSolution parseInput part1
  printSolutions 15 $ MkAoCSolution parseInput part2

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

sumGpsCoordinates :: Grid -> Int
sumGpsCoordinates = sum . map gpsCoordinates . M.keys . M.filter (== 'O')

gpsCoordinates :: Point -> Int
gpsCoordinates (V2 x y) = x + 100 * y

makeMove :: Grid -> Point -> Grid
makeMove g direction = if result == '#' then g else shiftAll g allPts
  where
    robot = findRobot g
    allPts = collectBoxes g [robot] direction
    result = M.findWithDefault ' ' (last allPts) g

factoryWarehouse :: Grid -> Warehouse
factoryWarehouse g = Warehouse (findRobot g) (findBoxes g) (M.keysSet $ M.filter (== '#') g)

scoreWarehouse :: Warehouse -> Int
scoreWarehouse = sum . map boxGpsCoordinates . boxes

boxGpsCoordinates :: Box -> Int
boxGpsCoordinates box = x + 100 * y
  where
    x = minimum $ map (^. _x) box
    y = minimum $ map (^. _y) box

makeMoveNew :: Warehouse -> Point -> Warehouse
makeMoveNew warehouse direction = updateWarehouse warehouse direction result
  where
    result = exploreMove warehouse direction [robot warehouse] []

exploreMove :: Warehouse -> Point -> [Point] -> [Box] -> MoveResult
exploreMove warehouse direction latestPoints currentBoxes
  | hitWall = MoveResult [] True
  | null newBoxes = MoveResult currentBoxes False
  | otherwise = exploreMove warehouse direction (concat newBoxes) (currentBoxes ++ newBoxes)
  where
    newPoints = map (+ direction) latestPoints
    hitWall = not $ null $ walls warehouse `S.intersection` S.fromList newPoints
    newBoxes = [box | box <- boxes warehouse, not $ null $ box `intersect` newPoints]

updateWarehouse :: Warehouse -> Point -> MoveResult -> Warehouse
updateWarehouse (Warehouse robot boxes walls) direction (MoveResult affectedBoxes hitWall)
  | hitWall = Warehouse robot boxes walls
  | otherwise = Warehouse (robot + direction) (shiftedBoxes ++ unaffectedBoxes) walls
  where
    unaffectedBoxes = boxes \\ affectedBoxes
    shiftedBoxes = map (shiftBox direction) affectedBoxes

shiftBox :: Point -> Box -> Box
shiftBox direction = map (+ direction)

-- Replace guard with ., last space with O, then second space with @ (overwriting O if there were no boxes)
shiftAll :: Grid -> [Point] -> Grid
shiftAll g pts = M.insert (head (tail pts)) '@' $ M.insert (last pts) 'O' $ M.insert (head pts) '.' g

collectBoxes :: Grid -> [Point] -> Point -> [Point]
collectBoxes g pts direction = if item == 'O' then collectBoxes g (pts ++ [nextPoint]) direction else pts ++ [nextPoint]
  where
    nextPoint = last pts + direction
    item = M.findWithDefault ' ' nextPoint g

findRobot :: Grid -> Point
findRobot = head . M.keys . M.filter (== '@')

findBoxes :: Grid -> [Box]
findBoxes = map singleton . M.keys . M.filter (== 'O')

part1 :: (Grid, [Point]) -> Int
part1 (g, moves) = sumGpsCoordinates $ foldl makeMove g moves

part2 :: (Grid, [Point]) -> Int
part2 (g, moves) = scoreWarehouse $ foldl makeMoveNew warehouse moves
  where
    warehouse = factoryWarehouse g
