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
import Data.Map as M (Map, map)
import Linear (V2 (..))
import Text.Trifecta (CharParsing (anyChar, string), Parser, TokenParsing (token), integer, many, some)
import Data.Char (digitToInt, intToDigit)
import Common.Debugging

type Position = Point

type Velocity = Point

data Dimensions = Dimensions {width :: Int, height :: Int}

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

part1 :: [Robot] -> Int
part1 = product . M.map length . associateBy (getQuadrant dimensions) . filter (hasQuadrant dimensions) . Prelude.map position . moveNTimes 100 dimensions
  where
    dimensions = Dimensions 101 103

part2 :: [Robot] -> Int
part2 = findPicture 0 dimensions
  where
    dimensions = Dimensions 101 103

makeMap :: [Robot] -> Grid
makeMap robots = M.map (intToDigit . length) $ associateBy id $ Prelude.map position robots

moveNTimes :: Int -> Dimensions -> [Robot] -> [Robot]
moveNTimes 0 dimensions robots = robots
moveNTimes n dimensions robots = moveNTimes (n - 1) dimensions newRobots
  where
    newRobots = moveRobots dimensions robots
    picturePotential = scorePicturePotential newRobots

findPicture :: Int -> Dimensions -> [Robot] -> Int
findPicture n dimensions robots = if picturePotential > 1 then traceVectorMap (makeMap newRobots) n+1 else findPicture (n+1) dimensions newRobots
  where
    newRobots = moveRobots dimensions robots
    picturePotential = scorePicturePotential newRobots


moveRobots :: Dimensions -> [Robot] -> [Robot]
moveRobots dimensions = Prelude.map (moveRobot dimensions)

scorePicturePotential :: [Robot] -> Int
scorePicturePotential robots = sum neighbours `div` length neighbours
  where 
    positions = Prelude.map position robots
    robotMap = makeMap robots
    neighbours = Prelude.map (length . gridNeighbours robotMap) positions

moveRobot :: Dimensions -> Robot -> Robot
moveRobot dimensions (Robot position velocity) = Robot (wrapAround dimensions $ position + velocity) velocity

wrapAround :: Dimensions -> Position -> Position
wrapAround (Dimensions width height) (V2 x y) = V2 (x `mod` width) (y `mod` height)

hasQuadrant :: Dimensions -> Position -> Bool
hasQuadrant (Dimensions width height) (V2 x y) = x /= width `div` 2 && y /= height `div` 2

getQuadrant :: Dimensions -> Position -> Quadrant
getQuadrant (Dimensions width height) (V2 x y)
  | x < xMid = if y < yMid then TL else BL
  | otherwise = if y < yMid then TR else BR
  where
    xMid = width `div` 2
    yMid = height `div` 2