module Solutions.Day12
  ( aoc12,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Common.Geometry
import qualified Data.Map as M
import Data.Set as S (Set, difference, fromList, null, toList, union, member, singleton, elemAt)
import Text.Trifecta (CharParsing (anyChar), Parser, many)
import Data.List (find, map, nub, sort)
import Control.Lens (none, (^.))
import Data.Maybe (isNothing, fromJust)
import           Linear.V2       (R1 (_x), R2 (_y), V2 (..))
import Common.ListUtils (associateBy, window2)

data Region = Region {plant :: Char, points :: S.Set Point}
  deriving (Show)

aoc12 :: IO ()
aoc12 = do
  printSolutions 12 $ MkAoCSolution parseInput part1
  printSolutions 12 $ MkAoCSolution parseInput part2

parseInput :: Parser Grid
parseInput = enumerateMultilineStringToVectorMap <$> many anyChar

regions :: Grid -> [Region]
regions g = findAllRegions g [] (S.fromList $ M.keys g)

findAllRegions :: Grid -> [Region] -> Set Point -> [Region]
findAllRegions g regions ptsRemaining
  | S.null ptsRemaining = regions
  | otherwise = findAllRegions g (regions ++ [newRegion]) (ptsRemaining `S.difference` points newRegion)
  where
    newPoint = S.elemAt 0 ptsRemaining
    newPlant = M.findWithDefault ' ' newPoint g
    newRegion = expandRegion g (Region newPlant $ S.singleton newPoint) (S.singleton newPoint)

expandRegion :: Grid -> Region -> S.Set Point -> Region
expandRegion grid (Region plant points) newPoints
  | S.null newNeighbours = Region plant points
  | otherwise = expandRegion grid newRegion newNeighbours
  where
    neighbours = S.fromList $ concatMap (M.keys . M.filter (== plant) . gridOrthogonalNeighbours grid) $ S.toList newPoints
    newNeighbours = S.difference neighbours points
    newRegion = Region plant $ S.union neighbours points

price :: Region -> Int
price (Region _ points) = length points * perimeter points

perimeter :: Set Point -> Int
perimeter regionPts = sum perimeters
  where
    allNeighbours = map allOrthogonalNeighbours $ S.toList regionPts
    perimeters = map (length . flip S.difference regionPts) allNeighbours

countSides :: Set Point -> Int
countSides pts = sum $ map (countSidesForDirection pts) allOrthogonalDirections

countSidesForDirection :: Set Point -> Point -> Int
countSidesForDirection pts (V2 x y) = sum $ M.map (distinctSides orthDirection) $ associateBy direction outliers
  where
    neighbours = S.fromList $ map (+ V2 x y) $ S.toList pts
    outliers = S.toList $ neighbours `S.difference` pts
    direction = if x == 0 then getY else getX
    orthDirection = if x == 0 then getX else getY

distinctSides :: (Point -> Int) -> [Point] -> Int
distinctSides fn pts = 1 + diffs
  where
    diffs = length $ filter (>1) $ map diff $ window2 $ sort $ map fn pts

getX :: Point -> Int
getX (V2 x y) = x

getY :: Point -> Int
getY (V2 x y) = y

diff :: (Int, Int) -> Int
diff (a, b) = b - a

priceB :: Region -> Int
priceB (Region _ points) = length points * countSides points

part1 :: Grid -> Int
part1 = sum . map price . regions

part2 :: Grid -> Int
part2 = sum . map priceB . regions
