module Solutions.Day14
  ( aoc14
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, many, CharParsing (anyChar, string), some, TokenParsing (token), integer)
import Common.Geometry
import Linear (V2(..))
import Common.ListUtils
import Data.Map as M (Map, map)


type Position = Point
type Velocity = Point

data Dimensions = Dimensions{width :: Int, height :: Int}
data Robot = Robot{position :: Position, velocity :: Velocity}
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
part1 = product . M.map length . associateBy (getQuadrant dimensions) . filter(hasQuadrant dimensions) . Prelude.map (position . moveNTimes 100 (moveRobot dimensions))
  where
    dimensions = Dimensions 101 103

part2 :: [Robot] -> String
part2 = undefined

moveNTimes :: Int -> (Robot -> Robot) -> Robot -> Robot
moveNTimes 0 moveFn robot = robot
moveNTimes n moveFn robot = moveNTimes (n-1) moveFn (moveFn robot)

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