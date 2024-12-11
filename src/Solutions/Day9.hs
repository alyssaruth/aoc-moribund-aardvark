module Solutions.Day9
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions, printTestSolutions,
  )
import Data.Char
import Text.Trifecta (CharParsing (anyChar), Parser, many)
import Control.Lens
import Data.List.Split
import Data.Maybe
import Data.Sequence (elemIndexR, fromList, Seq, mapWithIndex, elemIndexL, update, deleteAt, findIndexR, index)

aoc9 :: IO ()
aoc9 = do
  printSolutions 9 $ MkAoCSolution parseInput part1
  printSolutions 9 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = many anyChar

--part1 :: String -> Int
--part1 :: String -> [Maybe Int]
part1 = checksum . compress . parseMap

parseMap :: String -> Seq (Maybe Int)
parseMap = fromList . concat . imap parseMapElement . chunksOf 2 . map digitToInt

parseMapElement :: Int -> [Int] -> [Maybe Int]
parseMapElement index [file, free] = replicate file (Just index) ++ replicate free Nothing
parseMapElement index [file] = replicate file (Just index)

checksum :: Seq (Maybe Int) -> Int
checksum = sum . mapWithIndex blockChecksum

blockChecksum :: Int -> Maybe Int -> Int
blockChecksum ix block = ix * fromMaybe 0 block

compress :: Seq (Maybe Int) -> Seq (Maybe Int)
compress x = if firstNothingIx == lastJustIx + 1 then x else compress $ update firstNothingIx lastJust $ update lastJustIx Nothing x
  where
    firstNothingIx = fromJust $ elemIndexL Nothing x
    lastJustIx = fromJust $ findIndexR isJust x
    lastJust = x `Data.Sequence.index` lastJustIx

part2 :: String -> String
part2 = undefined
