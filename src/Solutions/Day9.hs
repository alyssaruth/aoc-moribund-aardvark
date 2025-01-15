module Solutions.Day9 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
  )
import Common.ListUtils (mapIf)
import qualified Common.MapUtils as S
import Control.Lens
import Data.Char (digitToInt)
import Data.List (find, delete)
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Sequence as S (Seq, drop, dropWhileL, elemIndexL, findIndicesL, fromList, index, length, mapWithIndex, replicate, reverse, take, update, (><), findIndexL)
import Text.Trifecta (Parser, alphaNum, many)
import qualified Data.Map as M
import Data.Foldable (toList)
import Debug.Trace (traceShow)

data File = File {fileId :: Int, size :: Int}
  deriving (Show, Eq, Ord)

type DiskMap = Seq (Maybe Int)

data DiskState = DiskState {gapGroups :: [[Int]], fileStarts :: M.Map (Maybe Int) Int, workingChecksum :: Int}

aoc9 :: IO ()
aoc9 = do
  printSolutions 9 'A' $ MkAoCSolution parseInput part1
  printSolutions 9 'B' $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = many alphaNum

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
part2 s = workingChecksum $ foldr tryMoveFile diskState $ imap parseFile $ chunksOf 2 $ map digitToInt s
  where
    startingMap = parseMap s
    gapGroups = groupGaps $ S.findIndicesL isNothing startingMap
    fileStarts = M.fromList $ Prelude.reverse $ toList $ mapWithIndex (\i fileId -> (fileId, i)) startingMap
    diskState = DiskState gapGroups fileStarts 0

-- [1, 2, 5, 6, 7, 11, 15, 16] -> [[1, 2], [5, 6, 7], [11], [15, 16]]
groupGaps :: [Int] -> [[Int]]
groupGaps gaps = makeGapGroups gaps [] []

makeGapGroups :: [Int] -> [[Int]] -> [Int] -> [[Int]]
makeGapGroups [] gapGroups currentGroup = gapGroups ++ [currentGroup]
makeGapGroups (nextGap:remainingGaps) gapGroups currentGroup
  | null currentGroup = makeGapGroups remainingGaps gapGroups [nextGap]
  | last currentGroup == nextGap - 1 = makeGapGroups remainingGaps gapGroups (currentGroup ++ [nextGap])
  | otherwise = makeGapGroups remainingGaps (gapGroups ++ [currentGroup]) [nextGap]

parseFile :: Int -> [Int] -> File
parseFile index (x : xs) = File index x

tryMoveFile :: File -> DiskState -> DiskState
tryMoveFile f (DiskState gapGroups fileStarts checksum)
  | isNothing targetGap = DiskState gapGroups fileStarts newChecksum
  | head (fromJust targetGap) > fileStartLocation = DiskState gapGroups fileStarts newChecksum
  | otherwise = moveFile f (fromJust targetGap) fileLocations (DiskState gapGroups fileStarts checksum)
  where
    fileStartLocation = fileStarts M.! Just (fileId f)
    targetGap = find (viableGap f fileStartLocation) gapGroups
    fileLocations = Prelude.take (size f) [fileStartLocation..]
    newChecksum = checksum + sum (map (* fileId f) fileLocations)

moveFile :: File -> [Int] -> [Int] -> DiskState -> DiskState
moveFile (File fileId size) targetGap fileLocations (DiskState gapGroups fileStarts checksum) 
  | null remainingGap = DiskState (delete targetGap gapGroups) fileStarts newChecksum
  | otherwise = DiskState updatedGroups fileStarts newChecksum
  where
    newIndices = Prelude.take size targetGap
    remainingGap = Prelude.drop size targetGap
    updatedGroups = mapIf (== targetGap) (const remainingGap) gapGroups
    newChecksum = checksum + sum (map (*fileId) newIndices)

viableGap :: File -> Int -> [Int] -> Bool
viableGap f currentLocation gap = Prelude.length gap >= size f
