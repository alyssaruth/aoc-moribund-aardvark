module Solutions.Day8
  ( aoc8
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, many, CharParsing (anyChar))
import Common.Geometry
    ( enumerateMultilineStringToVectorMap,
      gridNeighbours,
      Grid,
      Point )
import qualified Data.Map as M
import Data.List
import Control.Lens ((^.))
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))

data Node = Node {frequency :: Char, location :: Point}
  deriving (Show, Eq, Ord)

aoc8 :: IO ()
aoc8 = do
  printTestSolutions 8 $ MkAoCSolution parseInput part1
  printSolutions 8 $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

--part1 :: Grid -> String
--part1 g = length $ filter (hasAntiNode (allNodes g)) $ M.keys g
part1 g = length $ filter (hasAntiNode (allNodes g)) $ M.keys g

--findAllNodes :: Grid -> Point -> [Node]
-- findAllNodes g p = M.keys $ gridNeighbours g p

-- exploreDirection :: Grid -> [Node] -> Point -> Point -> [Node]
-- exploreDirection g nodes pt direction = if next == ' ' then nodes else exploreDirection g (nodes ++ node) nextPt direction
--   where
--     nextPt = pt + direction
--     next = M.findWithDefault ' ' nextPt g
--     node = [Node next nextPt | next `notElem` [' ', '.']]

allNodes :: Grid -> [Node]
allNodes g = [Node frequency pt | (pt, frequency) <- M.toList $ M.filter (/= '.') g]

groupNodes :: [Node] -> M.Map Char [Point]
groupNodes nodes = M.fromListWith (++) [(frequency n, [location n]) | n <- nodes]

-- inRange :: Point -> Node -> Bool
-- inRange pt node = x == 0 || y == 0 || abs x == abs y
--   where 
--     diff = distance pt (location node)
--     x = diff ^. _x
--     y = diff ^. _y


hasAntiNode :: [Node] -> Point  -> Bool
hasAntiNode nodes pt = any (isAntiNode pt) $ groupNodes nodes

isAntiNode :: Point -> [Point] -> Bool
isAntiNode p nodes = any (hasAntiNodePartner distances) distances
  where distances = map (distance p) nodes

distance :: Point -> Point -> Point
distance p q = p - q

hasAntiNodePartner :: [Point] -> Point -> Bool
hasAntiNodePartner nodes pt = 2*pt `elem` nodes

part2 :: Grid -> String
part2 = undefined
