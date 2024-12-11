module Solutions.Day9 where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Control.Lens
import Data.Char (digitToInt)
import Data.List.Split
import Data.Maybe
import Data.Sequence as S (Seq, deleteAt, drop, dropWhileL, elemIndexL, elemIndexR, findIndexR, findIndicesL, fromList, index, length, mapWithIndex, reverse, update)
import Text.Trifecta (CharParsing (anyChar), Parser, many)

aoc9 :: IO ()
aoc9 = do
  printSolutions 9 $ MkAoCSolution parseInput part1
  printSolutions 9 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = many anyChar

part1 :: String -> Int
part1 = checksum . doCompression . parseMap

parseMap :: String -> Seq (Maybe Int)
parseMap = fromList . concat . imap parseMapElement . chunksOf 2 . map digitToInt

parseMapElement :: Int -> [Int] -> [Maybe Int]
parseMapElement index [file, free] = replicate file (Just index) ++ replicate free Nothing
parseMapElement index [file] = replicate file (Just index)

checksum :: Seq (Maybe Int) -> Int
checksum = sum . mapWithIndex blockChecksum

blockChecksum :: Int -> Maybe Int -> Int
blockChecksum ix block = ix * fromMaybe 0 block

doCompression :: Seq (Maybe Int) -> Seq (Maybe Int)
doCompression x = S.reverse $ compress gaps $ S.reverse x
  where
    gaps = S.findIndicesL isNothing x

compress :: [Int] -> Seq (Maybe Int) -> Seq (Maybe Int)
compress gaps x
  | isNothing $ Nothing `S.elemIndexL` x = x -- No spaces left, done
  | isNothing (S.index x 0) = compress gaps $ dropWhileL isNothing x -- Drop all leading spaces
  | otherwise = compress (tail gaps) $ S.drop 1 $ S.update (S.length x - head gaps - 1) (S.index x 0) x -- Move current element into furthest gap

part2 :: String -> String
part2 = undefined
