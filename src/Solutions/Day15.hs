module Solutions.Day15
  ( aoc15,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry
import qualified Data.Map as M
import Linear (V2 (..))
import Text.Trifecta (CharParsing (anyChar, string), Parser, Parsing (try), TokenParsing (token), manyTill, oneOf, some)

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

part1 :: (Grid, [Point]) -> Int
part1 (g, moves) = sumGpsCoordinates $ foldl makeMove g moves

part2 :: (Grid, [Point]) -> String
part2 = undefined
