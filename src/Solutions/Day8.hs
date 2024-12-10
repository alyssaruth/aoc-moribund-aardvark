module Solutions.Day8
  ( aoc8,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Common.Geometry
  ( Grid,
    Point,
    enumerateMultilineStringToVectorMap,
  )
import Data.List
import qualified Data.Map as M
import Linear (V2 (..))
import Text.Trifecta (CharParsing (anyChar), Parser, many)

data Node = Node {frequency :: Char, location :: Point}
  deriving (Show, Eq, Ord)

type AntiNodeChecker = Point -> [Point] -> Bool

aoc8 :: IO ()
aoc8 = do
  printSolutions 8 $ MkAoCSolution parseInput part1
  printSolutions 8 $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

part1 :: Grid -> Int
part1 = length . findAntiNodes isAntiNode

findAntiNodes :: AntiNodeChecker -> Grid -> [Point]
findAntiNodes checker g = filter (hasAntiNode checker (groupedNodes g)) $ M.keys g

groupedNodes :: Grid -> M.Map Char [Point]
groupedNodes g = groupNodes $ [Node frequency pt | (pt, frequency) <- M.toList $ M.filter (/= '.') g]

groupNodes :: [Node] -> M.Map Char [Point]
groupNodes nodes = M.fromListWith (++) [(frequency n, [location n]) | n <- nodes]

hasAntiNode :: AntiNodeChecker -> M.Map Char [Point] -> Point -> Bool
hasAntiNode checker nodes pt = any (checker pt) nodes

isAntiNode :: Point -> [Point] -> Bool
isAntiNode p nodes = any (hasAntiNodePartner distances) distances
  where
    distances = filter (/= V2 0 0) $ map (p -) nodes

hasAntiNodePartner :: [Point] -> Point -> Bool
hasAntiNodePartner nodes pt = 2 * pt `elem` nodes

part2 :: Grid -> Int
part2 = length . findAntiNodes isAntiNodeB

isAntiNodeB :: Point -> [Point] -> Bool
isAntiNodeB p nodes = length normalDistances < length distances || p `elem` nodes
  where
    distances = filter (/= V2 0 0) $ map (p -) nodes
    normalDistances = nub $ map normalize distances

normalize :: Point -> Point
normalize (V2 x y) = V2 (x `div` divisor) (y `div` divisor)
  where
    divisor = gcd x y
