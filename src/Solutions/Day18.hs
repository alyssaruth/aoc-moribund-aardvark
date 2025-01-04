module Solutions.Day18
  ( aoc18,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Common.Geometry
import Common.ListUtils (associateBy)
import qualified Data.List
import qualified Data.Map as M
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Debug.Trace (traceShow)
import Linear (V2 (..))
import Text.Trifecta (Parser, TokenParsing (token), integer, some, string)

data Route = Route {visited :: Set Position, position :: Position}

width = 70

height = 70

aBytes = 1024

goal = V2 width height

aoc18 :: IO ()
aoc18 = do
  printSolutions 18 'A' $ MkAoCSolution parseInput part1
  printSolutions 18 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser [Point]
parseInput = do
  some $ token parsePoint

parsePoint :: Parser Point
parsePoint = do
  x <- integer
  y <- string "," *> integer
  pure $ V2 (fromInteger x) (fromInteger y)

startingRoute :: Int -> [Point] -> Route
startingRoute bytes input = Route (S.insert startingPosition walls) startingPosition
  where
    startingPosition = V2 0 0
    walls = S.fromList $ take bytes input

iterateRoutes :: M.Map Position Int -> [Route] -> [Route]
iterateRoutes scoreTable routes
  | Prelude.null unfinishedRoutes = routes
  | otherwise = iterateRoutes newScoreTable prunedRoutes
  where
    unfinishedRoutes = [route | route <- routes, position route /= goal]
    newRoutes = concatMap iterateRoute routes
    prunedRoutes = pruneRoutes scoreTable newRoutes
    newScoreTable = updateScores goal scoreTable prunedRoutes

pruneRoutes :: M.Map Position Int -> [Route] -> [Route]
pruneRoutes map routes = M.elems $ M.map head $ associateBy position highScoringRoutes
  where
    highScoringRoutes = [route | route <- routes, routeLength 0 route < highscore map route]

highscore :: M.Map Position Int -> Route -> Int
highscore map route = M.findWithDefault 10000000000 (position route) map

updateScores :: Position -> M.Map Position Int -> [Route] -> M.Map Position Int
updateScores goal scoreTable routes = M.union (M.fromList scorePairs) scoreTable
  where
    unfinishedRoutes = [route | route <- routes, position route /= goal]
    scorePairs = map scorePair unfinishedRoutes

scorePair :: Route -> (Position, Int)
scorePair route = (position route, routeLength 0 route)

iterateRoute :: Route -> [Route]
iterateRoute route
  | position route == goal = [route]
  | S.null validNeighbours = []
  | otherwise = map (updateRoute route) $ S.toList validNeighbours
  where
    validNeighbours = S.filter validPoint $ allOrthogonalNeighbours (position route) \\ visited route

validPoint :: Position -> Bool
validPoint (V2 x y) = x >= 0 && x <= width && y >= 0 && y <= height

updateRoute :: Route -> Position -> Route
updateRoute (Route visited position) newPt = Route (S.insert newPt visited) newPt

part1 :: [Point] -> Int
part1 input = minimum $ map (routeLength aBytes) $ iterateRoutes M.empty [startingRoute aBytes input]

routeLength :: Int -> Route -> Int
routeLength bytes route = S.size (visited route) - bytes - 1

findBlockage :: Int -> Int -> [Point] -> Int
findBlockage minBytes maxBytes input
  | minBytes == bytes || maxBytes == bytes = bytes
  | Data.List.null finishedRoutes = findBlockage minBytes bytes input
  | otherwise = findBlockage bytes maxBytes input
  where
    bytes = (minBytes + maxBytes) `div` 2
    finishedRoutes = iterateRoutes M.empty [startingRoute bytes input]

part2 :: [Point] -> String
part2 input = show x ++ "," ++ show y
  where
    blockageIndex = findBlockage aBytes (length input) input
    V2 x y = input !! blockageIndex
