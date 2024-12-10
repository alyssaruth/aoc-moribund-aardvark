module Solutions.Day8
  ( aoc8,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry
  ( Grid,
    Point,
    enumerateMultilineStringToVectorMap,
    gridNeighbours,
  )
import Data.List
import qualified Data.Map as M
import Text.Trifecta (CharParsing (anyChar), Parser, many)
import Linear (V2(..))

data Node = Node {frequency :: Char, location :: Point}
  deriving (Show, Eq, Ord)

aoc8 :: IO ()
aoc8 = do
  printTestSolutions 8 $ MkAoCSolution parseInput part1
  printSolutions 8 $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

part1 :: Grid -> Int
part1 g = length $ filter (hasAntiNode (allNodes g)) $ M.keys g

allNodes :: Grid -> [Node]
allNodes g = [Node frequency pt | (pt, frequency) <- M.toList $ M.filter (/= '.') g]

groupNodes :: [Node] -> M.Map Char [Point]
groupNodes nodes = M.fromListWith (++) [(frequency n, [location n]) | n <- nodes]

hasAntiNode :: [Node] -> Point -> Bool
hasAntiNode nodes pt = any (isAntiNode pt) $ groupNodes nodes

isAntiNode :: Point -> [Point] -> Bool
isAntiNode p nodes = any (hasAntiNodePartner distances) distances
  where
    distances = filter (/= V2 0 0) $ map (distance p) nodes

distance :: Point -> Point -> Point
distance p q = p - q

hasAntiNodePartner :: [Point] -> Point -> Bool
hasAntiNodePartner nodes pt = 2 * pt `elem` nodes

part2 :: Grid -> Int
part2 g = length $ filter (hasAntiNodeB (allNodes g)) $ M.keys g

hasAntiNodeB :: [Node] -> Point -> Bool
hasAntiNodeB nodes pt = any (isAntiNodeB pt) $ groupNodes nodes

isAntiNodeB :: Point -> [Point] -> Bool
isAntiNodeB p nodes = length normalDistances < length distances || p `elem` nodes
  where
    distances = filter (/= V2 0 0) $ map (distance p) nodes
    normalDistances = nub $ map normalize distances

normalize :: Point -> Point
normalize (V2 x y) = V2 (x `div` divisor) (y `div` divisor)
  where
    divisor = gcd x y
