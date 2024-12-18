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
import Data.Set as S ( Set, (\\), insert, null, toList, unions )
import qualified Data.Map as M
import Linear (V2(..))

data Route = Route{visited :: Set Position, score :: Int, position :: Position, direction :: Direction}

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

iterateRoutes :: M.Map Position Int -> Position -> [Route] -> [Route]
iterateRoutes scoreTable goal routes 
  | Prelude.null unfinishedRoutes = routes 
  | otherwise = iterateRoutes newScoreTable goal prunedRoutes
  where
    unfinishedRoutes = [route | route <- routes, position route /= goal]
    newRoutes = concatMap (iterateRoute goal) routes
    prunedRoutes = pruneRoutes scoreTable newRoutes
    newScoreTable = updateScores goal scoreTable prunedRoutes

updateScores :: Position -> M.Map Position Int -> [Route] -> M.Map Position Int
updateScores goal scoreTable routes = M.union (M.fromList scorePairs) scoreTable
  where 
    unfinishedRoutes = [route | route <- routes, position route /= goal]
    scorePairs = map scorePair unfinishedRoutes

pruneRoutes :: M.Map Position Int -> [Route] -> [Route]
pruneRoutes map routes = [route | route <- routes, score route < highscore map route]

highscore :: M.Map Position Int -> Route -> Int
highscore map route = M.findWithDefault 10000000000 (position route) map

scorePair :: Route -> (Position, Int)
scorePair route = (position route, score route)

iterateRoute :: Position -> Route -> [Route]
iterateRoute goal route
  | position route == goal = [route]
  | S.null validNeighbours = []
  | otherwise = map (updateRoute route) $ S.toList validNeighbours
  where
    validNeighbours = allOrthogonalNeighbours (position route) \\ visited route

updateRoute :: Route -> Position -> Route
updateRoute (Route visited score position direction) newPt = Route (insert newPt visited) (score+penalty) newPt newDirection
  where
    newDirection = newPt - position
    penalty = computePenalty position direction newPt

computePenalty :: Position -> Direction -> Position -> Int
computePenalty position direction newPosition
  | newPosition == position + direction = 1
  | otherwise = 1001

target :: Grid -> Position
target = locate 'E'

walls :: Grid -> Set Position
walls = M.keysSet . M.filter (== '#')

part1 :: Grid -> Int
part1 g = minimum $ map score $ iterateRoutes M.empty (target g) [startingRoute g]

part2 :: Grid -> Int
part2 g = length $ S.unions bestRouteVisits \\ walls g
  where
    finishedRoutes = iterateRoutes M.empty (target g) [startingRoute g]
    bestScore = minimum $ map score finishedRoutes
    bestRouteVisits = [visited route | route <- finishedRoutes, score route == bestScore]
