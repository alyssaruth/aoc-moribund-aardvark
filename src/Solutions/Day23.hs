module Solutions.Day23 (aoc23) where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Data.List (intercalate, intersect, isPrefixOf, nub)
import qualified Data.Map as M
import Data.Set (Set, delete, fromList, isSubsetOf, toList)
import Text.Trifecta (Parser, TokenParsing (token), alphaNum, count, some, string)

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

findSubnetworks :: M.Map Computer [Computer] -> [Set Computer]
findSubnetworks network = expandTriples (M.keys network) network []

expandTriples :: [Computer] -> M.Map Computer [Computer] -> [Set Computer] -> [Set Computer]
expandTriples computersRemaining network triplesSoFar
  | null computersRemaining = triplesSoFar
  | otherwise = expandTriples (tail computersRemaining) newMap (triplesSoFar ++ newTriples)
  where
    nextComputer = head computersRemaining
    newTriples = findTriplesForComputer nextComputer network
    newMap = purgeComputer nextComputer network

purgeComputer :: Computer -> M.Map Computer [Computer] -> M.Map Computer [Computer]
purgeComputer comp network = M.map (filter (/= comp)) $ M.delete comp network

findTriplesForComputer :: Computer -> M.Map Computer [Computer] -> [Set Computer]
findTriplesForComputer comp network = nub $ concatMap (findTriplesForPair network comp) connectedComps
  where
    connectedComps = network M.! comp

findTriplesForPair :: M.Map Computer [Computer] -> Computer -> Computer -> [Set Computer]
findTriplesForPair network a b = map (\c -> fromList [a, b, c]) sharedConnections
  where
    sharedConnections = (network M.! a) `intersect` (network M.! b)

part1 :: [(Computer, Computer)] -> Int
part1 = length . filter canContainChiefHistorian . findSubnetworks . makeConnectionsMap

canContainChiefHistorian :: Set Computer -> Bool
canContainChiefHistorian = any (isPrefixOf "t")

part2 :: [(Computer, Computer)] -> String
part2 pairs = intercalate "," $ toList $ findLargestSubnetwork network initialSubnetworks
  where
    network = makeConnectionsMap pairs
    initialSubnetworks = map (fromList . uncurry (:)) $ M.toList network

findLargestSubnetwork :: M.Map Computer [Computer] -> [Set Computer] -> Set Computer
findLargestSubnetwork network subnetworks
  | not (null validSubnetworks) = head validSubnetworks
  | otherwise = findLargestSubnetwork network $ concatMap splitSubnetwork subnetworks
  where
    validSubnetworks = filter (isValidSubnetwork network) subnetworks

splitSubnetwork :: Set Computer -> [Set Computer]
splitSubnetwork subnetwork = map (`delete` subnetwork) (toList subnetwork)

isValidSubnetwork :: M.Map Computer [Computer] -> Set Computer -> Bool
isValidSubnetwork networkMap subnetwork = all (isFullyConnected networkMap subnetwork) (toList subnetwork)

isFullyConnected :: M.Map Computer [Computer] -> Set Computer -> Computer -> Bool
isFullyConnected networkMap subnetwork computer = delete computer subnetwork `isSubsetOf` myConnections
  where
    myConnections = fromList $ networkMap M.! computer