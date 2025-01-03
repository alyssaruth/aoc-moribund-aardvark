module Solutions.Day12
  ( aoc12,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Common.Geometry
import Common.ListUtils (associateBy, window2)
import Data.List (map, sort)
import qualified Data.Map as M
import Data.Set as S (Set, difference, elemAt, fromList, null, singleton, toList, union, empty)
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Text.Trifecta (CharParsing (anyChar), Parser, many)
import Control.Lens ((^.))

type Region = S.Set Point

type Line = [Point]

aoc12 :: IO ()
aoc12 = do
  printSolutions 12 'A' $ MkAoCSolution parseInput part1
  printSolutions 12 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

regions :: Grid -> [Region]
regions g = findAllRegions g [] (M.keysSet g)

findAllRegions :: Grid -> [Region] -> Set Point -> [Region]
findAllRegions g regions ptsRemaining
  | S.null ptsRemaining = regions
  | otherwise = findAllRegions g (regions ++ [newRegion]) (ptsRemaining `S.difference` newRegion)
  where
    newPoint = S.elemAt 0 ptsRemaining
    newPlant = M.findWithDefault ' ' newPoint g
    newRegion = expandRegion g newPlant (S.singleton newPoint) (S.singleton newPoint)

expandRegion :: Grid -> Char -> Region -> Region -> Region
expandRegion grid plant points newPoints
  | S.null newNeighbours = points
  | otherwise = expandRegion grid plant newRegion newNeighbours
  where
    neighbours = S.fromList $ concatMap (M.keys . M.filter (== plant) . gridOrthogonalNeighbours grid) $ S.toList newPoints
    newNeighbours = S.difference neighbours points
    newRegion = S.union neighbours points

perimeter :: Region -> Int
perimeter regionPts = sum perimeters
  where
    allNeighbours = map allOrthogonalNeighbours $ S.toList regionPts
    perimeters = map (length . flip S.difference regionPts) allNeighbours

countSides :: Region -> Int
countSides pts = sum $ map (countSidesForDirection pts) allOrthogonalDirections

countSidesForDirection :: Region -> Point -> Int
countSidesForDirection pts (V2 x y) = sum $ M.map (distinctSides orthDirection) $ associateBy direction outliers
  where
    neighbours = S.fromList $ map (+ V2 x y) $ S.toList pts
    outliers = S.toList $ neighbours `S.difference` pts
    direction = if x == 0 then (^. _y) else (^. _x)
    orthDirection = if x == 0 then (^. _x) else (^. _y)

distinctSides :: (Point -> Int) -> Line -> Int
distinctSides fn line = 1 + gaps
  where
    gaps = length $ filter (> 1) $ map (uncurry (flip (-))) $ window2 $ sort $ map fn line

part1 :: Grid -> Int
part1 = sum . map (\x -> length x * perimeter x) . regions

part2 :: Grid -> Int
part2 = sum . map (\x -> length x * countSides x) . regions
