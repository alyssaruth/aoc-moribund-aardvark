module Solutions.Day9 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.ListUtils (mapIf)
import qualified Common.MapUtils as S
import Control.Lens
import Data.Char (digitToInt)
import Data.List (find)
import Data.List.Split
import Data.Maybe
import Data.Sequence as S (Seq, deleteAt, drop, dropWhileL, elemIndexL, elemIndexR, findIndexR, findIndicesL, fromList, index, length, mapWithIndex, replicate, reverse, take, update, (><))
import Debug.Trace (traceShow)
import Text.Trifecta (CharParsing (anyChar), Parser, many)

data File = File {fileId :: Int, size :: Int}
  deriving (Show, Eq, Ord)

type DiskMap = Seq (Maybe Int)

data DiskState = DiskState {diskMap :: DiskMap, gapGroups :: [[Int]]}

aoc9 :: IO ()
aoc9 = do
  printSolutions 9 $ MkAoCSolution parseInput part1
  printSolutions 9 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = many anyChar

part1 :: String -> Int
part1 = checksum . doCompression . parseMap

parseMap :: String -> DiskMap
parseMap = fromList . concat . imap parseMapElement . chunksOf 2 . map digitToInt

parseMapElement :: Int -> [Int] -> [Maybe Int]
parseMapElement index [file, free] = Prelude.replicate file (Just index) ++ Prelude.replicate free Nothing
parseMapElement index [file] = Prelude.replicate file (Just index)

checksum :: DiskMap -> Int
checksum = sum . mapWithIndex blockChecksum

blockChecksum :: Int -> Maybe Int -> Int
blockChecksum ix block = ix * fromMaybe 0 block

doCompression :: DiskMap -> DiskMap
doCompression x = S.reverse $ compress gaps $ S.reverse x
  where
    gaps = S.findIndicesL isNothing x

compress :: [Int] -> DiskMap -> DiskMap
compress gaps x
  | isNothing $ Nothing `S.elemIndexL` x = x -- No spaces left, done
  | isNothing (S.index x 0) = compress gaps $ dropWhileL isNothing x -- Drop all leading spaces
  | otherwise = compress (tail gaps) $ S.drop 1 $ S.update (S.length x - 1 - head gaps) (S.index x 0) x -- Move current element into furthest gap

part2 :: String -> Int
part2 s = checksum $ diskMap $ foldr tryMoveFile diskState $ imap parseFile $ chunksOf 2 $ map digitToInt s
  where
    diskState = DiskState (parseMap s) (groupGaps $ S.findIndicesL isNothing (parseMap s))

-- [1, 2, 5, 6, 7, 11, 15, 16] -> [[1, 2], [5, 6, 7], [11], [15, 16]]
groupGaps :: [Int] -> [[Int]]
groupGaps = foldl processGap []

processGap :: [[Int]] -> Int -> [[Int]]
processGap [] gap = [[gap]]
processGap gapGroups gap = if gap == last currentGroup + 1 then init gapGroups ++ [currentGroup ++ [gap]] else gapGroups ++ [[gap]]
  where
    currentGroup = last gapGroups

parseFile :: Int -> [Int] -> File
parseFile index (x : xs) = File index x

tryMoveFile :: File -> DiskState -> DiskState
tryMoveFile f (DiskState diskMap gapGroups)
  | isNothing targetGap = DiskState diskMap gapGroups
  | head (fromJust targetGap) > head fileLocations = DiskState diskMap gapGroups
  | otherwise = moveFile f (fromJust targetGap) fileLocations (DiskState diskMap gapGroups)
  where
    targetGap = find (viableGap f) gapGroups
    fileLocations = S.findIndicesL (== Just (fileId f)) diskMap

moveFile :: File -> [Int] -> [Int] -> DiskState -> DiskState
moveFile (File fileId size) targetGap fileLocations (DiskState diskMap gapGroups) = DiskState updatedMap updatedGroups
  where
    updatedMap = updateAll (Prelude.take size targetGap) (Just fileId) $ updateAll fileLocations Nothing diskMap
    remainingGap = Prelude.drop size targetGap
    updatedGroups = mapIf (== targetGap) (const remainingGap) gapGroups

updateAll :: [Int] -> Maybe Int -> DiskMap -> DiskMap
updateAll indices newValue seq = start >< middle >< end
  where
    start = S.take (head indices) seq
    middle = S.replicate (Prelude.length indices) newValue
    end = S.drop (last indices + 1) seq

viableGap :: File -> [Int] -> Bool
viableGap f gap = Prelude.length gap >= size f
