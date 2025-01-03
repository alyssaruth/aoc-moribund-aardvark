module Solutions.Day14
  ( aoc14,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Common.Geometry
import Common.ListUtils
import Data.Map as M (map)
import qualified Data.Set as S
import Linear (V2 (..))
import Text.Trifecta (CharParsing (string), Parser, TokenParsing (token), integer, some)

type Velocity = Point

data Robot = Robot {position :: Position, velocity :: Velocity}
  deriving (Show)

data Quadrant = TL | TR | BL | BR
  deriving (Eq, Show, Ord)

aoc14 :: IO ()
aoc14 = do
  printSolutions 14 'A' $ MkAoCSolution parseInput part1
  printSolutions 14 'B' $ MkAoCSolution parseInput part2

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

-- makeMap :: [Robot] -> Grid
-- makeMap robots = M.fromList $ [(position, '*') | (Robot position _) <- robots]

moveNTimes :: Int -> [Robot] -> [Robot]
moveNTimes 0 robots = robots
moveNTimes n robots = moveNTimes (n - 1) newRobots
  where
    newRobots = moveRobots robots

findPicture :: Int -> [Robot] -> Int
-- findPicture n robots = if containsTriangle newRobots then traceVectorMap (makeMap newRobots) n+1 else findPicture (n+1) newRobots
findPicture n robots = if containsTriangle newRobots then n + 1 else findPicture (n + 1) newRobots
  where
    newRobots = moveRobots robots

-- Look for an arrangement of robots like:
--
--   X
--  XXX
-- XXXXX
--
containsTriangle :: [Robot] -> Bool
containsTriangle robots = any (topOfTriangle pts) pts
  where
    pts = S.fromList $ Prelude.map position robots

topOfTriangle :: S.Set Position -> Position -> Bool
topOfTriangle robots (V2 x y) = all (`S.member` robots) (lineOne ++ lineTwo)
  where
    lineOne = Prelude.map (\xDiff -> V2 (x + xDiff) y + 1) [-1, 0, 1]
    lineTwo = Prelude.map (\xDiff -> V2 (x + xDiff) y + 2) [-2, -1, 0, 1, 2]

moveRobots :: [Robot] -> [Robot]
moveRobots = Prelude.map moveRobot

moveRobot :: Robot -> Robot
moveRobot (Robot position velocity) = Robot (wrapAround $ position + velocity) velocity

wrapAround :: Position -> Position
wrapAround (V2 x y) = V2 (x `mod` width) (y `mod` height)

hasQuadrant :: Position -> Bool
hasQuadrant (V2 x y) = x /= xMid && y /= yMid

getQuadrant :: Position -> Quadrant
getQuadrant (V2 x y)
  | x < xMid = if y < yMid then TL else BL
  | otherwise = if y < yMid then TR else BR