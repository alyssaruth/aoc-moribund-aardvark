{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Solutions.Day23
  ( aoc23
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, alphaNum, count, string, some, TokenParsing (token))
import qualified Data.Map as M
import Data.List (intersect, (\\), nub, isPrefixOf)
import Data.Set (Set, fromList)

type Computer = String

aoc23 :: IO ()
aoc23 = do
  printSolutions 23 $ MkAoCSolution parseInput part1
  printSolutions 23 $ MkAoCSolution parseInput part2

parseInput :: Parser [(Computer, Computer)]
parseInput = do
  some $ token parseConnection

parseConnection :: Parser (Computer, Computer)
parseConnection = do
  x <- count 2 alphaNum
  y <- string "-" *> count 2 alphaNum
  pure (x, y)

makeConnectionsMap :: [(Computer, Computer)] -> M.Map Computer [Computer]
makeConnectionsMap pairs = M.fromListWith (++) $ concatMap (\(a, b) -> [(a, [b]), (b, [a])]) pairs

findTriples :: M.Map Computer [Computer] -> [Set Computer]
findTriples network = expandTriples (M.keys network) network []

expandTriples :: [Computer] -> M.Map Computer [Computer] -> [Set Computer] -> [Set Computer]
expandTriples computersRemaining network triplesSoFar
  | null computersRemaining = triplesSoFar
  | otherwise = expandTriples (tail computersRemaining) newMap (triplesSoFar ++ newTriples)
  where
    nextComputer = head computersRemaining
    newTriples = findTriplesForComputer nextComputer network
    newMap = purgeComputer nextComputer network

purgeComputer :: Computer -> M.Map Computer [Computer] -> M.Map Computer [Computer]
purgeComputer comp network = M.map (filter (/=comp)) $ M.delete comp network

findTriplesForComputer :: Computer -> M.Map Computer [Computer] -> [Set Computer]
findTriplesForComputer comp network = nub $ concatMap (findTriplesForPair network comp) connectedComps
  where
    connectedComps = network M.! comp

findTriplesForPair :: M.Map Computer [Computer] -> Computer -> Computer -> [Set Computer]
findTriplesForPair network a b = map (\c -> fromList [a, b, c]) sharedConnections
  where
    sharedConnections = (network M.! a) `intersect` (network M.! b)

part1 :: [(Computer, Computer)] -> Int
part1 = length . filter canContainChiefHistorian . findTriples . makeConnectionsMap

canContainChiefHistorian :: Set Computer -> Bool
canContainChiefHistorian = any (isPrefixOf "t")

part2 :: [(Computer, Computer)] -> String
part2 = undefined
