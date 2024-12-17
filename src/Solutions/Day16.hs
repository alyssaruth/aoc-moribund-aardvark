module Solutions.Day16
  ( aoc16,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Text.Trifecta (Parser, many, CharParsing (anyChar))
import Common.Geometry
import Data.Set as S ( Set, (\\), insert, null, toList )
import qualified Data.Map as M
import Linear (V2(..))
import Debug.Trace

data Route = Route{visited :: Set Point, score :: Int, position :: Point, direction :: Point}

aoc16 :: IO ()
aoc16 = do
  printSolutions 16 $ MkAoCSolution parseInput part1
  printSolutions 16 $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

startingRoute :: Grid -> Route
startingRoute g = Route (insert position (walls g)) 0 position (V2 1 0)
  where
    position = locate 'S' g

iterateRoutes :: Point -> [Route] -> [Route]
iterateRoutes goal routes = if Prelude.null unfinishedRoutes then routes else traceShow debugStr iterateRoutes goal (concatMap (iterateRoute goal) routes)
  where
    unfinishedRoutes = [route | route <- routes, position route /= goal]
    debugStr = "Total [" ++ show (length routes) ++ "], unfinished [" ++ show (length unfinishedRoutes) ++ "]"

iterateRoute :: Point -> Route -> [Route]
iterateRoute goal route
  | position route == goal = [route]
  | S.null validNeighbours = []
  | otherwise = map (updateRoute route) $ S.toList validNeighbours
  where
    validNeighbours = allOrthogonalNeighbours (position route) \\ visited route

updateRoute :: Route -> Point -> Route
updateRoute (Route visited score position direction) newPt = Route (insert newPt visited) (score+penalty) newPt newDirection
  where
    newDirection = newPt - position
    penalty = computePenalty position direction newPt

computePenalty :: Point -> Point -> Point -> Int
computePenalty position direction newPosition
  | newPosition == position + direction = 1
  | newPosition == position - direction = 2001
  | otherwise = 1001

target :: Grid -> Point
target = locate 'E'

walls :: Grid -> Set Point
walls = M.keysSet . M.filter (== '#')

part1 :: Grid -> Int
part1 g = minimum $ map score $ iterateRoutes (target g) [startingRoute g]

part2 :: Grid -> String
part2 = undefined
