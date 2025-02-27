module Common.Geometry where

import           Control.Lens    ((^.))
import           Data.Foldable   (maximumBy, minimumBy)
import           Data.List.Split (chunksOf)
import qualified Data.Map        as M hiding (mapMaybe)
import           Data.Maybe      (mapMaybe)
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq
import qualified Data.Set        as S
import           Linear.V2       (R1 (_x), R2 (_y), V2 (..))
import Linear (unit)

type Point = V2 Int

type Direction = Point

type Position = Point

type Grid = M.Map Point Char

enumerateMultilineString :: String -> [((Int, Int), Char)]
enumerateMultilineString str
  | maximum lengths /= minimum lengths = error "Line lengths are not equal"
  | otherwise = zip coords (concat lines')
  where
    lines' = lines str
    xLength = length (head lines')
    yLength = length lines'
    lengths = map length lines'
    coords = [(x, y) | y <- [0 .. yLength - 1], x <- [0 .. xLength - 1]]

enumerateMultilineStringToVectorMap :: String -> M.Map (V2 Int) Char
enumerateMultilineStringToVectorMap =
  M.fromList . map (\((x, y), c) -> (V2 x y, c)) . enumerateMultilineString

gridNeighbours :: Grid -> Point -> Grid
gridNeighbours grid point = M.restrictKeys grid $ neighbours point

gridNeighboursInclusive :: Grid -> Point -> Grid
gridNeighboursInclusive grid point = M.restrictKeys grid $ S.fromList $ point : neighboursL point

neighboursL :: Point -> [Point]
neighboursL point = map (+ point) directions
  where
    directions = [V2 x y | x <- units, y <- units, [x, y] /= [0, 0]]
    units = [-1, 0, 1]

neighbours :: Point -> S.Set Point
neighbours point = S.fromList $ neighboursL point

locate :: Char -> Grid -> Point
locate c = head . M.keys . M.filter (==c)

renderVectorMap :: M.Map (V2 Int) Char -> String
renderVectorMap m =
  if null m
    then ""
    else rendered
  where
    keys = M.keys m
    xMax = maximumBy (\a b -> compare (a ^. _x) (b ^. _x)) keys ^. _x
    xMin = minimumBy (\a b -> compare (a ^. _x) (b ^. _x)) keys ^. _x
    yMax = maximumBy (\a b -> compare (a ^. _y) (b ^. _y)) keys ^. _y
    yMin = minimumBy (\a b -> compare (a ^. _y) (b ^. _y)) keys ^. _y
    xRange = (xMax - xMin) + 1
    panelList =
      [ M.findWithDefault '.' (V2 x y) m
      | y <- [yMin .. yMax]
      , x <- [xMin .. xMax]
      ]
    panelRows = chunksOf xRange panelList
    rendered = unlines panelRows

renderVectorSet :: S.Set Point -> String
renderVectorSet points =
  let asMap = M.fromSet (const '#') points
   in renderVectorMap asMap

renderVectorList :: [Point] -> String
renderVectorList = renderVectorSet . S.fromList

allOrthogonalDirections :: [V2 Int]
allOrthogonalDirections = [unit _x, -unit _x, unit _y, -unit _y]

allOrthogonalNeighbours :: V2 Int -> S.Set Point
allOrthogonalNeighbours v = S.fromList $ map (v +) allOrthogonalDirections

gridOrthogonalNeighbours :: Grid -> Point -> Grid
gridOrthogonalNeighbours grid point = M.restrictKeys grid $ allOrthogonalNeighbours point

normalize :: Point -> Point
normalize (V2 x y) = V2 (x `div` divisor) (y `div` divisor)
  where
    divisor = gcd x y