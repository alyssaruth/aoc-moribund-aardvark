module Solutions.Day14
  ( aoc14,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry
import Common.ListUtils
import Data.Map as M (Map, map, fromList)
import Linear (V2 (..))
import Text.Trifecta (CharParsing (string), Parser, TokenParsing (token), integer, some)
import Common.Debugging

type Velocity = Point

data Robot = Robot {position :: Position, velocity :: Velocity}
  deriving (Show)

data Quadrant = TL | TR | BL | BR
  deriving (Eq, Show, Ord)

aoc14 :: IO ()
aoc14 = do
  printSolutions 14 $ MkAoCSolution parseInput part1
  printSolutions 14 $ MkAoCSolution parseInput part2

parseInput :: Parser [Robot]
parseInput = do
  some $ token parseRobot

parseRobot :: Parser Robot
parseRobot = do
  x <- string "p=" *> integer
  y <- string "," *> integer
  v_x <- string "v=" *> integer
  v_y <- string "," *> integer
  pure $ Robot (V2 (fromInteger x) (fromInteger y)) (V2 (fromInteger v_x) (fromInteger v_y))


width = 101
height = 103
xMid = width `div` 2
yMid = height `div` 2

part1 :: [Robot] -> Int
part1 = product . M.map length . associateBy getQuadrant . filter hasQuadrant . Prelude.map position . moveNTimes 100

part2 :: [Robot] -> Int
part2 = findPicture 0

makeMap :: [Robot] -> Grid
makeMap robots = M.fromList $ [(position, '*') | (Robot position _) <- robots]

moveNTimes :: Int -> [Robot] -> [Robot]
moveNTimes 0 robots = robots
moveNTimes n robots = moveNTimes (n - 1) newRobots
  where
    newRobots = moveRobots robots

findPicture :: Int -> [Robot] -> Int
findPicture n robots = if picturePotential then traceVectorMap (makeMap newRobots) n+1 else findPicture (n+1) newRobots
  where
    newRobots = moveRobots robots
    picturePotential = averageNeighbours newRobots > 1

moveRobots :: [Robot] -> [Robot]
moveRobots = Prelude.map moveRobot

averageNeighbours :: [Robot] -> Int
averageNeighbours robots = sum neighbours `div` length neighbours
  where 
    positions = Prelude.map position robots
    robotMap = makeMap robots
    neighbours = Prelude.map (length . gridNeighbours robotMap) positions

moveRobot ::  Robot -> Robot
moveRobot (Robot position velocity) = Robot (wrapAround $ position + velocity) velocity

wrapAround :: Position -> Position
wrapAround (V2 x y) = V2 (x `mod` width) (y `mod` height)

hasQuadrant :: Position -> Bool
hasQuadrant (V2 x y) = x /= xMid && y /= yMid

getQuadrant :: Position -> Quadrant
getQuadrant (V2 x y)
  | x < xMid = if y < yMid then TL else BL
  | otherwise = if y < yMid then TR else BR