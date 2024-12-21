module Solutions.Day17
  ( aoc17
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, CharParsing (string), integer, commaSep)
import Debug.Trace
import Data.Bits (xor)
import Data.List (genericLength, isPrefixOf)

data Computer = Computer{a :: Integer, b :: Integer, c :: Integer, program :: [Integer], pointer :: Integer, outputs :: [Integer]}
  deriving (Show)

aoc17 :: IO ()
aoc17 = do
  printSolutions 17 $ MkAoCSolution parseInput part1
  printSolutions 17 $ MkAoCSolution parseInput part2

parseInput :: Parser Computer
parseInput = do
  a <- string "Register A: " *> integer
  b <- string "Register B: " *> integer
  c <- string "Register C: " *> integer
  program <- string "Program: " *> commaSep integer
  pure $ Computer a b c program 0 []

combo :: Computer -> Integer -> Integer
combo comp operand
 | operand < 4 = operand
 | operand == 4 = a comp
 | operand == 5 = b comp
 | operand == 6 = c comp
 | otherwise = traceShow "Invalid combo operand" undefined

processInstruction :: Computer -> Computer
processInstruction comp
  | opCode == 0 = adv comp
  | opCode == 1 = bxl comp
  | opCode == 2 = bst comp
  | opCode == 3 = jnz comp
  | opCode == 4 = bxc comp
  | opCode == 5 = out comp
  | opCode == 6 = bdv comp
  | opCode == 7 = cdv comp
  where
    opCode = program comp !! fromInteger (pointer comp)

adv :: Computer -> Computer
adv comp = divisionInstruction comp updateA

bxl :: Computer -> Computer
bxl comp = movePointer $ updateB comp (b comp `xor` readOperand comp)

bst :: Computer -> Computer
bst comp = movePointer $ updateB comp (readComboOperand comp `mod` 8)

jnz :: Computer -> Computer
jnz comp
  | a comp == 0 = movePointer comp
  | otherwise = updatePointer comp (readOperand comp)

bxc :: Computer -> Computer
bxc comp = movePointer $ updateB comp (b comp `xor` c comp)

out :: Computer -> Computer
out comp = movePointer $ addOutput comp (readComboOperand comp `mod` 8)

bdv :: Computer -> Computer
bdv comp = divisionInstruction comp updateB

cdv :: Computer -> Computer
cdv comp = divisionInstruction comp updateC

divisionInstruction :: Computer -> (Computer -> Integer -> Computer) -> Computer
divisionInstruction comp updater = movePointer $ updater comp (a comp `div` (2 ^ readComboOperand comp))

readComboOperand :: Computer -> Integer
readComboOperand comp = combo comp (readOperand comp)

readOperand :: Computer -> Integer
readOperand c = program c !! fromInteger (pointer c + 1)

movePointer :: Computer -> Computer
movePointer comp = updatePointer comp (pointer comp + 2)

addOutput :: Computer -> Integer -> Computer
addOutput (Computer a b c program pointer output) new = Computer a b c program pointer (output++[new])

updatePointer :: Computer -> Integer -> Computer
updatePointer (Computer a b c program _ output) new = Computer a b c program new output

updateA :: Computer -> Integer -> Computer
updateA (Computer _ b c program pointer output) new = Computer new b c program pointer output

updateB :: Computer -> Integer -> Computer
updateB (Computer a _ c program pointer output) new = Computer a new c program pointer output

updateC :: Computer -> Integer -> Computer
updateC (Computer a b _ program pointer output) new = Computer a b new program pointer output


runProgram :: Computer -> Computer
runProgram comp = if pointer result >= genericLength (program result) then result else runProgram result
  where
    result = processInstruction comp

part1 :: Computer -> [Integer]
part1 = outputs . runProgram

findCorrectA :: Computer -> Integer
findCorrectA comp
  | outputs result == program result = a comp
  | otherwise = traceShow (show (a comp) ++ ": " ++ show result) $ findCorrectA $ updateA comp (a comp + 8^10)
  -- | otherwise = findCorrectA $ updateA comp (a comp + 8)
  where
    result = runProgram comp

part2 :: Computer -> Integer
part2 comp = findCorrectA $ updateA comp (8^15)
