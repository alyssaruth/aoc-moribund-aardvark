module Solutions.Day2
  ( aoc2
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Control.Applicative.Combinators (some)
import qualified Data.Map as Map
import           Text.Parser.Char    (newline)
import           Text.Trifecta       (CharParsing (string), Parser,
                                      commaSep, integer, letter,
                                      semiSep)

data Colour = Red | Green | Blue
    deriving (Eq, Show, Bounded, Enum, Ord)

type CubeRound = Map.Map Colour Integer

data CubeGame = CubeGame { gameId :: Integer, rounds :: [CubeRound] }

aoc2 :: IO ()
aoc2 = do
  printSolutions 2 $ MkAoCSolution parseInput part1
  printSolutions 2 $ MkAoCSolution parseInput part2

parseInput :: Parser [CubeGame]
parseInput = some $ parseGame <* newline

parseGame :: Parser CubeGame
parseGame = do
  label <- string "Game " *> integer <* string ": "
  rounds <- fmap (map Map.fromList) $ semiSep $ commaSep parseCubes
  pure $ CubeGame label rounds

parseCubes :: Parser (Colour, Integer)
parseCubes = do
  num <- integer
  c <- some letter
  case c of
    "red"   -> return (Red, num)
    "green" -> return (Green, num)
    "blue"  -> return (Blue, num)
    _       -> fail $ "Invalid colour: " ++ c

part1 :: [CubeGame] -> Integer
part1 = sum . map gameId . filter isValidA

part2 :: [CubeGame] -> Integer
part2 = sum . map power

isValidA :: CubeGame -> Bool
isValidA = all isValidRoundA . rounds

isValidRoundA :: CubeRound -> Bool
isValidRoundA r = hasAtMost r Red 12 && hasAtMost r Green 13 && hasAtMost r Blue 14

hasAtMost :: CubeRound -> Colour -> Integer -> Bool
hasAtMost r c threshold = countColour c r <= threshold

power :: CubeGame -> Integer
power g = (fewestCubes Red g) * (fewestCubes Blue g) * (fewestCubes Green g)

fewestCubes :: Colour -> CubeGame -> Integer
fewestCubes c g = maximum ( map (countColour c) (rounds g) )

countColour :: Colour -> CubeRound  -> Integer
countColour c r = Map.findWithDefault 0 c r
